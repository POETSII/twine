#!/bin/bash

export PATH=$PATH:$(realpath ../bin)

if [ ! -f "../rtl/mkCore" ]; then
  echo Please build the simulator first
  exit -1
fi

NCYCLES=10000
TEST_DIR=$(realpath .)
make --quiet
for FILE in *.S; do
  TEST=$(basename $FILE .S)
  echo -ne "$TEST\t"
  cp $TEST.code.hex ../rtl/InstrMem.hex
  cp $TEST.data.hex ../rtl/DataMem.hex
  pushd . > /dev/null
  cd ../rtl
  RESULT=$(./mkCore -m $NCYCLES | grep -v -E '^Warning' | head -n 1)
  popd > /dev/null
  if [ $(($RESULT)) == "1" ]; then
    echo "PASSED"
  else
    NUM=$((16#$RESULT/2))
    echo "FAILED #$NUM"
    exit -1
  fi
done
