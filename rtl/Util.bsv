package Util;

import DReg      :: *;
import ConfigReg :: *;
import Vector    :: *;

// Useful function for constructing a mux with a one-hot select
function t when(Bool b, t x)
    provisos (Bits#(t, tWidth), Add#(_, 1, tWidth));
  return unpack(signExtend(pack(b)) & pack(x));
endfunction

// Mux with a one-hot selector
function t oneHotSelect(Vector#(n, Bool) oneHot, Vector#(n, t) vec)
    provisos (Bits#(t, tWidth),
              Add#(_a, 1, tWidth),
              Add#(_b, 1, n));
  return unpack(fold( \| , zipWith(when, oneHot, map(pack, vec))));
endfunction

// Binary encoder: convert from one-hot to binary
function Bit#(n) encode(Vector#(TExp#(n), Bool) oneHot)
  provisos (Add#(_a, 1, n),
            Add#(_b, 1, TExp#(n)));
  return oneHotSelect(oneHot, map(fromInteger, genVector));
endfunction

// Are all bits high?
function Bool allHigh(Bit#(n) x) = unpack(reduceAnd(x));

// Are all bits low?
function Bool allLow(Bit#(n) x) = !unpack(reduceOr(x));

// Are all bools high?
function Bool andVec(Vector#(n, Bool) bools) = allHigh(pack(bools));

// Assertion
function Action myAssert(Bool b, String s) =
  action
    if (!b && genC()) begin
      $display("Assertion failed: ", s);
      $finish();
    end
  endaction;

// Alternative encoding of the Maybe type
typedef struct {
  Bool valid;
  t value;
} Option#(type t) deriving (Bits);

// Friendly constructor for Option type
function Option#(t) option(Bool valid, t value) =
  Option { valid: valid, value: value };

// Set/reset flip-flop
interface SetReset;
  method Action set;
  method Action clear;
  method Bool value;
endinterface

module mkSetReset#(Bool init) (SetReset);
  Reg#(Bool) state <- mkConfigReg(init);
  PulseWire setWire <- mkPulseWire;
  PulseWire clearWire <- mkPulseWire;

  rule update;
    if (setWire)
      state <= True;
    else if (clearWire)
      state <= False;
  endrule

  method Action set;
    setWire.send;
  endmethod

  method Action clear;
    clearWire.send;
  endmethod

  method Bool value = state;
endmodule

// A VReg is a register that can only be read
// on the clock cycle after it is written
module mkVReg (Reg#(t)) provisos (Bits#(t, twidth));
  Reg#(t) register <- mkRegU;
  Wire#(t) valWire <- mkDWire(?);
  Reg#(Bool) valid <- mkDReg(False);

  rule update;
    register <= valWire;
  endrule

  method Action _write (t val);
    valWire <= val;
    valid   <= True;
  endmethod

  method t _read if (valid) = register;
endmodule

endpackage
