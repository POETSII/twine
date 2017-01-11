package CoreLib;

// ============================================================================
// Imports
// ============================================================================

import Vector    :: *;
import Util      :: *;

// ============================================================================
// Types
// ============================================================================

// Core-local thread id
typedef Bit#(`LogThreadsPerCore) ThreadId;

// An index to instruction memory
typedef Bit#(`LogInstrsPerCore) InstrIndex;

// A byte-address in instruction memory
typedef Bit#(TAdd#(`LogInstrsPerCore, 2)) InstrAddr;

// For each thread, we keep the following info
typedef struct {
  // Program counter
  InstrAddr pc;
  // Thread identifier
  ThreadId id;
} ThreadState deriving (Bits);

// Register file index
// (Register file constains 32 registers per thread)
typedef Bit#(TAdd#(`LogThreadsPerCore, 5)) RegFileIndex;

// RV32I instruction type (one-hot encoding)
typedef struct {
  Bool isRType;  Bool isIType;
  Bool isSType;  Bool isSBType;
  Bool isUType;  Bool isUJType;
} InstrType deriving (Bits);

// Decoded CSR
typedef struct {
  `ifdef SIMULATE
  Bool isEmit;
  `endif
  Bool isHartId;
} CSR deriving (Bits);

// Decoded operation
typedef struct {
  Bool isAdd;            Bool isSub;
  Bool isSetIfLessThan;  Bool isShiftLeft;
  Bool isShiftRight;     Bool isAnd;
  Bool isOr;             Bool isXor;
  Bool isOpUI;           Bool isJump;
  Bool isJumpReg;
  Bool isBranchEq;       Bool isBranchNotEq;
  Bool isBranchLessThan; Bool isBranchGreaterOrEqualTo;
  Bool isLoad;           Bool isStore;
  Bool isCSR;            CSR csr;
  Bool isAddOrSub;       Bool isBitwise;
} Op deriving (Bits);

// Instruction result
typedef struct {
  Bit#(33) add;       Bit#(32) csr;
  Bit#(32) shiftLeft; Bit#(32) shiftRight;
  Bit#(32) bitwise;   Bit#(32) opui;
} InstrResult deriving (Bits);

// Width of load or store access
typedef struct {
  // Byte, half-word, and full-word accesses
  Bool b; Bool h; Bool w;
} AccessWidth deriving (Bits);

// The type for data passed between each pipeline stage
typedef struct {
  ThreadState thread;      // Current thread state
  Bit#(32) instr;          // RV32I-encoded instruction
  Bit#(32) valA;           // Value of 1st register operand
  Bit#(32) valB;           // Value of 2nd register operand
  Bit#(32) imm;            // Immediate operand
  Bit#(32) aluB;           // Second operand to ALU
  Bool writeRegFile;       // Enable writeback to register file
  Bit#(32) writeVal;       // Value to write to register file
  InstrType instrType;     // RV32I instruction type
  AccessWidth accessWidth; // Byte, half-word, or word access?
  Bit#(32) loadVal;        // Result of load instruction
  Bit#(32) memAddr;        // Memory address for load or store
  Op op;                   // Decoded operation
  InstrResult instrResult; // Instruction result
  InstrAddr jumpBase;      // Base of jump relative (PC or register)
  InstrAddr targetPC;      // Next PC if branch taken
  InstrAddr nextPC;        // Next PC if branch not taken
  Bool retry;              // Instruction needs retried later
} PipelineToken deriving (Bits);

// For each suspended thread, we have the following info
typedef struct {
  ThreadState thread;      // Thread state
  Bit#(5) destReg;         // Destination register for the load result
  Bit#(2) loadSelector;    // Bottom two bits of load address
  AccessWidth accessWidth; // Access width of load (byte, half, word)
  Bool isUnsignedLoad;     // Sign-extension behaviour for load
} SuspThreadState deriving (Bits);

// ============================================================================
// Decoder
// ============================================================================

// RV32I instruction fields
function Bit#(7) opcode(Bit#(32) instr)  = instr[6:0];
function Bit#(5) rd(Bit#(32) instr)      = instr[11:7];
function Bit#(3) funct3(Bit#(32) instr)  = instr[14:12];
function Bit#(5) rs1(Bit#(32) instr)     = instr[19:15];
function Bit#(5) rs2(Bit#(32) instr)     = instr[24:20];
function Bit#(7) funct7(Bit#(32) instr)  = instr[31:25];

// Compute immediate for each type of RV32I instruction
function Bit#(32) immI(Bit#(32) i) =
  signExtend({i[31],i[30:20]});
function Bit#(32) immS(Bit#(32) i) =
  signExtend({i[31],i[30:25],i[11:8],i[7]});
function Bit#(32) immB(Bit#(32) i) =
  signExtend({i[31],i[7],i[30:25],i[11:8],1'b0});
function Bit#(32) immU(Bit#(32) i) =
  {i[31],i[30:20],i[19:12],12'b0};
function Bit#(32) immJ(Bit#(32) i) =
  signExtend({i[31],i[19:12],i[20],i[30:25],i[24:21],1'b0});

// Determine instruction type from instruction
function InstrType decodeInstrType(Bit#(32) instr);
  Bit#(5) op = instr[6:2];
  InstrType t;
  t.isRType  = op == 'b01100;      /* Arithmetic */
  t.isIType  = op == 'b00100       /* Arithmetic-immediate */
            || op == 'b00000       /* Loads */
            || op == 'b11001       /* JALR */
            || op == 'b00011       /* Fences */
            || op == 'b11100;      /* System */
  t.isSType  = op == 'b01000;      /* Stores */
  t.isSBType = op == 'b11000;      /* Branches */
  t.isUType  = op == 'b01101       /* LUI */
            || op == 'b00101;      /* AUIPC */
  t.isUJType = op == 'b11011;      /* JAL */
  return t;
endfunction

// Determine immediate operand from instruction and type
function Bit#(32) decodeImm(Bit#(32) instr, InstrType t);
  return when(t.isIType , immI(instr))
       | when(t.isSType , immS(instr))
       | when(t.isSBType, immB(instr))
       | when(t.isUType , immU(instr))
       | when(t.isUJType, immJ(instr));
endfunction

// Decode operation
function Op decodeOp(Bit#(32) instr);
  Op ret = ?;
  Bit#(5) op = instr[6:2];
  Bit#(3) minorOp = funct3(instr);
  // Arithmetic operations
  Bool isArithReg = op == 'b01100; // Second arg a register
  Bool isArithImm = op == 'b00100; // Second arg an immediate
  Bool isArith = isArithReg || isArithImm;
  ret.isAdd = minorOp == 'b000 && (isArithImm || isArithReg && instr[30] == 0);
  ret.isSub = minorOp == 'b000 && isArithReg && instr[30] == 1;
  ret.isAddOrSub = ret.isAdd || ret.isSub;
  ret.isSetIfLessThan = (minorOp == 'b010 || minorOp == 'b011) && isArith;
  ret.isShiftLeft = minorOp == 'b001 && isArith;
  ret.isShiftRight = minorOp == 'b101 && isArith;
  ret.isAnd = minorOp == 'b111 && isArith;
  ret.isOr = minorOp == 'b110 && isArith;
  ret.isXor = minorOp == 'b100 && isArith;
  ret.isBitwise = ret.isAnd || ret.isOr || ret.isXor;
  // Load or add-to upper immediate
  ret.isOpUI = op == 'b01101 || op == 'b00101;
  // Jump operations
  ret.isJump = op == 'b11011 || op == 'b11001;
  ret.isJumpReg = op == 'b11001;
  // Branch operations
  Bool isBranch = op == 'b11000;
  ret.isBranchEq = isBranch && minorOp == 'b000;
  ret.isBranchNotEq = isBranch && minorOp == 'b001;
  ret.isBranchLessThan =
    isBranch && (minorOp == 'b100 || minorOp == 'b110);
  ret.isBranchGreaterOrEqualTo =
    isBranch && (minorOp == 'b101 || minorOp == 'b111);
  // Load & store operations
  ret.isLoad = op == 'b00000;
  ret.isStore = op == 'b01000;
  // CSR read/write operation
  ret.isCSR = op == 'b11100;
  Bit#(4) csrIndex = instr[23:20];
  // Hardware thread id CSR
  ret.csr.isHartId = ret.isCSR && csrIndex == 'h4;
  // Other CSRs
  `ifdef SIMULATE
  ret.csr.isEmit = ret.isCSR && csrIndex == 'h0;
  `endif
  return ret;
endfunction

// Is second ALU operand an immediate?
function Bool isALUImm(Bit#(32) instr) = !unpack(instr[5]);

// Is comparison signed or unsigned
function Bool isUnsignedCmp(Bit#(32) instr) =
     funct3(instr) == 3'b011   // SLTU
  || funct3(instr) == 3'b110   // BLTU
  || funct3(instr) == 3'b111;  // BGEU

// Is shift an arithmetic (sign-preserving) shift?
function Bool isArithShift(Bit#(32) instr) =
  funct7(instr)[5] == 1;

// Add PC to upper immediate?
function Bool addPCtoUI(Bit#(32) instr) =
  instr[5] == 0;

// Compute width of load or store access
function AccessWidth decodeAccessWidth(Bit#(32) instr);
  AccessWidth access;
  Bit#(2) w = funct3(instr)[1:0];
  access.w = w == 2;
  access.h = w == 1;
  access.b = w == 0;
  return access;
endfunction

// Is load signed or unsigned?
function Bool isUnsignedLoad(Bit#(32) instr) = unpack(funct3(instr)[2]);

// Does operation write to register file?
function Bool isRegFileWrite(Op op) =
     op.isAdd            || op.isSub
  || op.isSetIfLessThan  || op.isShiftLeft
  || op.isShiftRight     || op.isAnd
  || op.isOr             || op.isXor
  || op.isOpUI           || op.isJump
  || op.isCSR;

// ==============
// Loads & Stores
// ==============

// Compute byte-enable given access width
// and bottom two bits of address
function Bit#(4) genByteEnable(AccessWidth access, Bit#(2) a);
  return when(access.w, 4'b1111)
       | when(access.h, {a[1],a[1],~a[1],~a[1]})
       | when(access.b, {pack(a==3),pack(a==2),pack(a==1),pack(a==0)});
endfunction

// Align a write using access width
function Bit#(32) writeAlign(AccessWidth access, Bit#(32) x);
  return when(access.w, x)
       | when(access.h, {x[15:0], x[15:0]})
       | when(access.b, {x[7:0], x[7:0], x[7:0], x[7:0]});
endfunction

// Compute loaded word using access width,
// bottom two bits of load address,
// and a flag indicating whether load is unsigned or not
function Bit#(32) loadMux(Bit#(32) x, AccessWidth access,
                          Bit#(2) a, Bool isUnsigned);
  Bit#(8)  b = case (a) matches
                 0: x[7:0];
                 1: x[15:8];
                 2: x[23:16];
                 3: x[31:24];
               endcase;
  Bit#(16) h = a[1] == 0 ? x[15:0] : x[31:16];
  return when(access.w, x)
       | when(access.h, {isUnsigned ? 0 : signExtend(h[15]), h})
       | when(access.b, {isUnsigned ? 0 : signExtend(b[7]), b});
endfunction

endpackage
