// Simple wrapper for an Avalon memory-mapped master interface.

package AvalonMaster;

// =============================================================================
// Imports
// =============================================================================

import Util      :: *;
import CoreLib   :: *;
import ConfigReg :: *;
import Vector    :: *;

// =============================================================================
// Interface
// =============================================================================

interface AvalonMasterExt;
  (* always_ready *)
  method Bit#(32) master_address;
  (* always_ready *)
  method Bit#(32) master_writedata;
  (* always_ready *)
  method Bool     master_write;
  (* always_ready *)
  method Bool     master_read;
  (* always_ready *)
  method Bit#(4)  master_byteenable;
  (* always_enabled *)
  method Action   master(Bool master_waitrequest,
                         Bit#(32) master_readdata);
endinterface

interface AvalonMaster;
  `ifndef SIMULATE
  // Avalon interface
  interface AvalonMasterExt avalonMasterExt;
  `endif

  // Requests
  method Bool canPutRead;
  method Bool canPutWrite;
  method Action put(SuspThreadState thread, Bool write,
                    Bit#(32) address, Bit#(32) data, Bit#(4) byteEn);
  // Responses
  method Bool canGet;
  method Action get;
  method Bit#(32) respData;
  method SuspThreadState respThread;
endinterface

// =============================================================================
// Synthesis version
// =============================================================================

`ifndef SIMULATE

// State of the wrapper
typedef enum {
  MASTER_IDLE,
  MASTER_READ,
  MASTER_WRITE
} AvalonMasterState deriving (Bits, Eq);

module mkAvalonMaster (AvalonMaster);

  // Response buffer (for now, a single element)
  SetReset              respRegFull   <- mkSetReset(False);
  Reg#(Bit#(32))        respReg       <- mkConfigRegU;
  Reg#(SuspThreadState) respRegThread <- mkConfigRegU;

  // State machine
  Reg#(AvalonMasterState) state <- mkConfigReg(MASTER_IDLE);
  Reg#(Bit#(32)) addressReg     <- mkConfigRegU;
  Reg#(Bit#(32)) dataReg        <- mkConfigRegU;
  Reg#(Bit#(4))  byteEnReg      <- mkConfigRegU;

  // Wires
  Wire#(AvalonMasterState) gotoState <- mkDWire(MASTER_IDLE);

  // Avalon memory-mapped interface
  interface AvalonMasterExt avalonMasterExt;
    method Bit#(32) master_address = addressReg;
    method Bit#(32) master_writedata = dataReg;
    method Bool     master_write = state == MASTER_WRITE;
    method Bool     master_read  = state == MASTER_READ;
    method Bit#(4)  master_byteenable = byteEnReg;

    method Action master(Bool master_waitrequest,
                         Bit#(32) master_readdata);
      case (state)
        MASTER_IDLE:
          state <= gotoState;
        MASTER_READ:
          if (!master_waitrequest) begin
            respRegFull.set;
            respReg <= master_readdata;
            state <= MASTER_IDLE;
          end
        MASTER_WRITE:
          if (!master_waitrequest) state <= MASTER_IDLE;
      endcase
    endmethod
  endinterface

  // Requests
  method Bool canPutRead = state == MASTER_IDLE && !respRegFull.value;
  method Bool canPutWrite = state == MASTER_IDLE;
  method Action put(SuspThreadState thread, Bool write,
                    Bit#(32) address, Bit#(32) data, Bit#(4) byteEn);
    gotoState <= write ? MASTER_WRITE : MASTER_READ;
    addressReg <= address;
    dataReg <= data;
    byteEnReg <= byteEn;
    respRegThread <= thread;
  endmethod

  // Responses
  method Bool canGet = respRegFull.value;
  method Action get;
    respRegFull.clear;
  endmethod
  method Bit#(32) respData = respReg;
  method SuspThreadState respThread = respRegThread;

endmodule

`endif

// =============================================================================
// Simulation verison
// =============================================================================

// In simulation, simply provide 4GB of RAM

`ifdef SIMULATE

// Interface to C functions for memory simulation
import "BDPI" function ActionValue#(Bit#(32)) ramRead(Bit#(32) addr);
import "BDPI" function Action ramWrite(
  Bit#(32) addr, Bit#(32) data, Bit#(32) bitEn);

module mkAvalonMaster (AvalonMaster);

  // Response buffer (for now, a single element)
  SetReset              respRegFull   <- mkSetReset(False);
  Reg#(Bit#(32))        respReg       <- mkConfigRegU;
  Reg#(SuspThreadState) respRegThread <- mkConfigRegU;

  // Requests
  method Bool canPutRead = !respRegFull.value;
  method Bool canPutWrite = True;
  method Action put(SuspThreadState thread, Bool write,
                    Bit#(32) address, Bit#(32) data, Bit#(4) byteEn);
    Vector#(4, Bit#(8)) en;
    for (Integer i = 0; i < 4; i=i+1) en[i] = signExtend(byteEn[i]);
    Bit#(32) bitEn = {en[3], en[2], en[1], en[0]};
    if (write)
      ramWrite(address, data, bitEn);
    else begin
      Bit#(32) val <- ramRead(address);
      respReg <= val;
      respRegThread <= thread;
      respRegFull.set;
    end
  endmethod

  // Responses
  method Bool canGet = respRegFull.value;
  method Action get;
    respRegFull.clear;
  endmethod
  method Bit#(32) respData = respReg;
  method SuspThreadState respThread = respRegThread;

endmodule

`endif

endpackage
