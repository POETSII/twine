// Copyright (c) Matthew Naylor

package Core;

// ============================================================================
// Imports
// ============================================================================

import Vector       :: *;
import BlockRam     :: *;
import Assert       :: *;
import Util         :: *;
import DReg         :: *;
import ConfigReg    :: *;
import AvalonMaster :: *;
import CoreLib      :: *;

// ============================================================================
// Core Interface
// ============================================================================

interface Core;
  `ifndef SIMULATE
  interface AvalonMasterExt avalonMaster;
  `endif
endinterface

// ============================================================================
// Pipeline 
// ============================================================================

// Diagram
// =======
//
//     +-----------+       +==========+
//     | Instr Mem |<----->| Schedule |
//     +-----------+       | & fetch  |<---------------------+
//                         +==========+                      |
//                             ||                            |
//                             \/                            |
//     +-----------+       +========+                        |
//  +->| Reg File  |<----->| Decode |                        |
//  |  +-----------+       +========+                        |
//  |                          ||                            |
//  |                          \/                            |
//  |                      +============+                    |
//  |                      | Execute    |                    |
//  |                      | or Suspend |                    |
//  |                      +============+                    |
//  |                          ||                            |
//  |                          \/                            |
//  |                      +============+                    |
//  +----------------------| Write Back |--------------------+
//                         +============+            

// Properties
// ==========
//
// Hazard-free: at most one instruction per thread in pipeline at any
// time.
//
// Non-blocking: if instruction accesses busy resource (e.g. memory)
// then it is retried later by requeueing into the run queue without
// incrementing the PC.
//
// Loads are suspended in the Execute stage and resumed in the
// Write Back stage.

(* synthesize *)
module mkCore (Core);

  staticAssert(`ThreadsPerCore >= 4, "Number of threads must be >= 4");

  // Global state
  // ------------

  // Avalon master interface
  AvalonMaster master <- mkAvalonMaster;

  // Runnable threads
  Vector#(`ThreadsPerCore, SetReset) runnable <- replicateM(mkSetReset(True));

  // Program counters
  Vector#(`ThreadsPerCore, Reg#(InstrAddr)) pc <- replicateM(mkConfigReg(0));

  // Instruction memory
  BlockRamOpts instrMemOpts = defaultBlockRamOpts;
  instrMemOpts.initFile = Valid("InstrMem");
  instrMemOpts.registerDataOut = False;
  BlockRam#(InstrIndex, Bit#(32)) instrMem <- mkBlockRamOpts(instrMemOpts);

  // Register file (duplicated to allow two reads per cycle)
  BlockRamOpts regFileOpts = defaultBlockRamOpts;
  regFileOpts.registerDataOut = False;
  BlockRam#(RegFileIndex, Bit#(32)) regFileA <- mkBlockRamOpts(regFileOpts);
  BlockRam#(RegFileIndex, Bit#(32)) regFileB <- mkBlockRamOpts(regFileOpts);

  // Pipeline stages
  Reg#(PipelineToken) decodeInput    <- mkVReg;
  Reg#(PipelineToken) executeInput   <- mkVReg;
  Reg#(Bool)          writebackFire  <- mkDReg(False);
  Reg#(PipelineToken) writebackInput <- mkRegU;
 
  // Fetch stage
  // -----------

  rule fetch;
    // Function to get value of SetReset flip-flop
    function Bool getValue(SetReset sr) = sr.value;
    // Get first runnable thread
    Bool found = False;
    ThreadId firstRunnable = ?;
    InstrAddr firstPC = ?;
    for (Integer i = 0; i < `ThreadsPerCore; i=i+1)
      if (!found && runnable[i].value) begin
        found         = True;
        firstRunnable = fromInteger(i);
        firstPC       = pc[i];
        runnable[i].clear;
      end
    // Fetch instruction for thread
    instrMem.read(truncateLSB(firstPC));
    // Create a pipeline token to hold new instruction
    PipelineToken token = ?;
    token.thread.id = firstRunnable;
    token.thread.pc = firstPC;
    // Trigger next stage
    if (found) decodeInput <= token;
  endrule

  // Decode stage
  // ------------

  rule decode;
    PipelineToken token = decodeInput;
    // Grab instruction memory outputs
    token.instr = instrMem.dataOut;
    // Fetch operands from register files
    regFileA.read({token.thread.id, rs1(token.instr)});
    regFileB.read({token.thread.id, rs2(token.instr)});
    // Compute instruction's operation and type
    token.op = decodeOp(token.instr);
    token.instrType = decodeInstrType(token.instr);
    // Compute access width of load or store
    token.accessWidth = decodeAccessWidth(token.instr);
    // Compute instruction's immediate
    token.imm = decodeImm(token.instr, token.instrType);
    // CSR-immediate instructions not yet supported
    if (token.op.isCSR)
      myAssert(token.instr[14] == 0, "CSR-immediate instrs not supported");
    // Trigger second decode sub-stage
    executeInput <= token;
  endrule

  // Execute stage
  // -------------

  rule execute;
    PipelineToken token = executeInput;
    // Grab register values
    token.valA = regFileA.dataOut;
    token.valB = regFileB.dataOut;
    // Compute ALU's second operand
    token.aluB = isALUImm(token.instr) ? token.imm : token.valB;
    // Determine memory address for load or store
    token.memAddr = token.valA + token.imm;
    // Base of jump (could be PC or register)
    token.jumpBase = token.op.isJumpReg ?
                       truncate(token.valA) : token.thread.pc;
    // Emit char to console (simulation only)
    `ifdef SIMULATE
    if (token.op.csr.isEmit) begin
      $display("%x", token.valA);
    end
    `endif

    // Determine result of instruction
    InstrResult res = ?;
    // 33-bit addition/subtraction (result used for comparisons too)
    Bool ucmp = isUnsignedCmp(token.instr);
    let addA = {ucmp ? 1'b0 : token.valA[31], token.valA};
    let addB = {ucmp ? 1'b0 : token.aluB[31], token.aluB};
    res.add = (addA + (token.op.isAdd ? addB : ~addB)) +
                (token.op.isAdd ? 0 : 1);
    // Shift left
    res.shiftLeft = token.valA << token.aluB[4:0];
    // Shift right (both logical and arithmetic cases)
    Bit#(1) shiftExt = isArithShift(token.instr) ? token.valA[31] : 1'b0;
    Int#(33) shiftRes = unpack({shiftExt, token.valA}) >> token.aluB[4:0];
    res.shiftRight = truncate(pack(shiftRes));
    // Bitwise operations
    res.bitwise = when (token.op.isAnd, token.valA & token.aluB)
                | when (token.op.isOr,  token.valA | token.aluB)
                | when (token.op.isXor, token.valA ^ token.aluB);
    // Load upper immediate (+ PC)
    res.opui = token.imm + (addPCtoUI(token.instr) ?
                              zeroExtend(token.thread.pc) : 0);
    // Load or store: send request to avalon master
    Bool retry = False;
    Bool suspend = False;
    if (token.op.isLoad || token.op.isStore) begin
      // Determine data to write and assoicated byte-enables
      Bit#(32) writeData = writeAlign(token.accessWidth, token.valB);
      Bit#(4)  byteEn    = genByteEnable(token.accessWidth, token.memAddr[1:0]);
      Bool masterCanPut  = token.op.isLoad ?
        master.canPutRead : master.canPutWrite;
      if (masterCanPut) begin
        // State of suspended thread
        SuspThreadState susp;
        susp.thread = token.thread;
        susp.thread.pc = token.thread.pc + 4;
        susp.destReg = rd(token.instr);
        susp.loadSelector = token.memAddr[1:0];
        susp.accessWidth = token.accessWidth;
        susp.isUnsignedLoad = isUnsignedLoad(token.instr);
        // Put request to avalon master
        master.put(susp, token.op.isStore, token.memAddr, writeData, byteEn);
        if (token.op.isLoad) suspend = True;
      end else
        retry = True;
    end
    // Compute next PC
    token.nextPC = token.thread.pc + (retry ? 0 : 4);
    // Compute jump/branch target
    token.targetPC = token.jumpBase + truncate(token.imm);
    token.targetPC[0] = token.op.isJumpReg ? 0 : token.targetPC[0];
    // CSR read
    res.csr = zeroExtend(token.thread.id);
    // Trigger next stage
    token.retry = retry;
    token.instrResult = res;
    writebackInput <= token;
    if (! suspend) writebackFire <= True;
  endrule

  // Write Back stage
  // ----------------

  rule writeback;
    PipelineToken token = writebackInput;
    // Compute results of comparison
    InstrResult res = token.instrResult;
    Bool eq = res.add == 0;
    Bool lt = res.add[32] == 1;
    // Setup write to destination register
    Op op = token.op;
    token.writeVal =
        when(op.isAddOrSub,       res.add[31:0])
      | when(op.isSetIfLessThan,  lt ? 1 : 0)
      | when(op.isShiftLeft,      res.shiftLeft)
      | when(op.isShiftRight,     res.shiftRight)
      | when(op.isBitwise,        res.bitwise)
      | when(op.isOpUI,           res.opui)
      | when(op.isJump,           zeroExtend(token.nextPC))
      | when(op.isCSR,            res.csr);
    // Setup new PC
    Bool takeBranch =
         op.isJump
      || (op.isBranchEq               && eq)
      || (op.isBranchNotEq            && !eq)
      || (op.isBranchLessThan         && lt)
      || (op.isBranchGreaterOrEqualTo && !lt);
    token.thread.pc = takeBranch ? token.targetPC : token.nextPC;
    // Write to register file?
    token.writeRegFile =
      isRegFileWrite(token.op) && rd(token.instr) != 0 && !token.retry;

    // Should we write to the register file?
    Bool writeToRegFile = False;
    // If so, what value and which destination?
    Bit#(32) writeVal = ?;
    RegFileIndex dest = ?;
    // Process an instruction from execute stage
    if (writebackFire) begin
      if (token.writeRegFile) begin
        writeToRegFile = True;
        writeVal = token.writeVal;
        dest = {token.thread.id, rd(token.instr)};
      end
    end 
    // Try to service a response from the avalon master
    SuspThreadState susp = master.respThread;
    // Are we resuming this thread?
    Bool resumeThread = False;
    if (master.canGet) begin
      // If register file's write-port is not in use then 
      // a pending load result can be written back
      if (!writeToRegFile) begin
        // Write to register file
        writeToRegFile = True;
        writeVal = loadMux(master.respData, susp.accessWidth,
                             susp.loadSelector, isUnsignedLoad(token.instr));
        dest = {susp.thread.id, susp.destReg};
        master.get;
        resumeThread = True;
      end
    end
    // Make the thread(s) runnable again
    for (Integer i = 0 ; i < `ThreadsPerCore; i=i+1) begin
      if (writebackFire && token.thread.id == fromInteger(i)) begin
        pc[i] <= token.thread.pc;
        runnable[i].set;
      end else if (resumeThread && susp.thread.id == fromInteger(i)) begin
        pc[i] <= susp.thread.pc;
        runnable[i].set;
      end
    end
    // Register file write
    if (writeToRegFile) begin
      regFileA.write(dest, writeVal);
      regFileB.write(dest, writeVal);
    end
  endrule

  // Interface
  // ---------

  `ifndef SIMULATE
  interface AvalonMaster   avalonMaster = master.avalonMasterExt;
  `endif

endmodule

endpackage
