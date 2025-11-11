#!/bin/bash

# Run from the root of the VerCors dir. 
# Put path to the spec2ir-repo here
SPEC2IR_ROOT=/home/rme/repos/pallas_spec2ir
SPEC2IR="${SPEC2IR_ROOT}/build/bin/pallasSpec2ir"
C_LIB_PATH=$SPEC2IR_ROOT/res/spec_libs/c
CPP_LIB_PATH=$SPEC2IR_ROOT/res/spec_libs/cpp
SWIFT_LIB_PATH=$SPEC2IR_ROOT/res/spec_libs/swift/PallasSpec/.build/debug/Modules
W_DIR=tmp/

translate_file() {
    local inFile="${1}"
    local outFile="${2}"
    local specLib="${3}"
    local args=$4
    echo "-------------------------------------------"
    echo " Translating $inFile --> $outFile"
    rm $outFile
    $SPEC2IR $inFile -o $outFile -lib $specLib -wDir $W_DIR $args
}

translate_c_file() {
  translate_file $1 $2 $C_LIB_PATH $3
}
translate_cpp_file() {
  translate_file $1 $2 $CPP_LIB_PATH $3
}
translate_swift_file() {
  translate_file $1 $2 $SWIFT_LIB_PATH $3
}

mkdir -p $W_DIR

# C Tests
echo "=============================================="
echo " Translating C Tests"
echo "=============================================="
translate_c_file examples/concepts/llvm/pallas/pallas_c_assert.c examples/concepts/llvm/pallas/pallas_c_assert.ll "-mem2reg"
translate_c_file examples/concepts/llvm/pallas/pallas_c_assert.c examples/concepts/llvm/pallas/pallas_c_assert.ll "-mem2reg"
translate_c_file examples/concepts/llvm/pallas/pallas_c_assert_fail.c examples/concepts/llvm/pallas/pallas_c_assert_fail.ll "-mem2reg"
translate_c_file examples/concepts/llvm/pallas/pallas_c_assume.c examples/concepts/llvm/pallas/pallas_c_assume.ll "-mem2reg"
translate_c_file examples/concepts/llvm/pallas/pallas_c_fibonacci.c examples/concepts/llvm/pallas/pallas_c_fibonacci.ll "-mem2reg"
translate_c_file examples/concepts/llvm/pallas/pallas_c_loop_unused.c examples/concepts/llvm/pallas/pallas_c_loop_unused.ll "-mem2reg"
translate_c_file examples/concepts/llvm/pallas/pallas_c_lower_bound.c examples/concepts/llvm/pallas/pallas_c_lower_bound.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_multiply.c examples/concepts/llvm/pallas/pallas_c_multiply.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_old.c examples/concepts/llvm/pallas/pallas_c_old.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_old_fail.c examples/concepts/llvm/pallas/pallas_c_old_fail.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_perm.c examples/concepts/llvm/pallas/pallas_c_perm.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_perm_fail_1.c examples/concepts/llvm/pallas/pallas_c_perm_fail_1.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_perm_fail_2.c examples/concepts/llvm/pallas/pallas_c_perm_fail_2.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_perm_fail_3.c examples/concepts/llvm/pallas/pallas_c_perm_fail_3.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_quantifier.c examples/concepts/llvm/pallas/pallas_c_quantifier.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_quantifier_fail.c examples/concepts/llvm/pallas/pallas_c_quantifier_fail.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_c_square_fail.c examples/concepts/llvm/pallas/pallas_c_square_fail.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_function_contract.c examples/concepts/llvm/pallas/pallas_function_contract.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_function_contract_fail.c examples/concepts/llvm/pallas/pallas_function_contract_fail.ll ""
translate_c_file examples/concepts/llvm/pallas/pallas_loop_goto.c examples/concepts/llvm/pallas/pallas_loop_goto.ll "-mem2reg"
translate_c_file examples/concepts/llvm/pallas/pallas_result.c examples/concepts/llvm/pallas/pallas_result.ll ""
translate_c_file examples/concepts/llvm/pallas/extContracts/pallas_c_genContrAssume.c examples/concepts/llvm/pallas/extContracts/pallas_c_genContrAssume.ll ""
translate_c_file examples/concepts/llvm/pointer_casts.c examples/concepts/llvm/pointer_casts.ll
translate_c_file examples/concepts/c/pointer_relations.c examples/concepts/llvm/pointer_relations.ll

# C++ Tests
echo "=============================================="
echo " Translating C++ Tests"
echo "=============================================="
translate_cpp_file examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.cpp examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.ll "--cLib=examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.h"
translate_cpp_file examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr.cpp examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr_fail.ll "--cLib=examples/concepts/llvm/pallas/extContracts/pallas_cpp_extContr_fail.h"
translate_cpp_file examples/concepts/llvm/pallas/extContracts/pallas_cpp_genContr.cpp examples/concepts/llvm/pallas/extContracts/pallas_cpp_genContr.ll

# Swift Tests
echo "=============================================="
echo " Translating Swift Tests"
echo "=============================================="
translate_swift_file examples/concepts/llvm/pallas/pallas_swift_assert.swift examples/concepts/llvm/pallas/pallas_swift_assert.ll "-mem2reg"
translate_swift_file examples/concepts/llvm/pallas/pallas_swift_fib.swift examples/concepts/llvm/pallas/pallas_swift_fib.ll "-mem2reg"
translate_swift_file examples/concepts/llvm/pallas/pallas_swift_fib_fail.swift examples/concepts/llvm/pallas/pallas_swift_fib_fail.ll "-mem2reg"

rm -r $W_DIR