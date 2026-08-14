#!/bin/bash

# Run from the root of the VerCors dir to re-translate the Spectal-examples.


# Put path to the spectral-repo here
SPEC2IR_ROOT=/home/rme/repos/pallas_spec2ir
SPEC2IR="${SPEC2IR_ROOT}/build/bin/pallasSpec2ir"
C_LIB_PATH=$SPEC2IR_ROOT/res/spec_libs/c
CPP_LIB_PATH=$SPEC2IR_ROOT/res/spec_libs/cpp
SWIFT_LIB_PATH=$SPEC2IR_ROOT/res/spec_libs/swift/PallasSpec/.build/debug/Modules
W_DIR=tmp_spectral/

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

echo "Clearing and creating working-directory"
rm -rf   $W_DIR
mkdir -p $W_DIR

echo "=============================================="
echo " Translating C Examples"
echo "=============================================="
translate_c_file examples/publications/2026/ATVA2026Spectral/C/cantor.c examples/publications/2026/ATVA2026Spectral/C/cantor.ll "-mem2reg"
translate_c_file examples/publications/2026/ATVA2026Spectral/C/date.c examples/publications/2026/ATVA2026Spectral/C/date.ll "-mem2reg"
translate_c_file examples/publications/2026/ATVA2026Spectral/C/fibonacci.c examples/publications/2026/ATVA2026Spectral/C/fibonacci.ll "-mem2reg"
translate_c_file examples/publications/2026/ATVA2026Spectral/C/sort.c examples/publications/2026/ATVA2026Spectral/C/sort.ll "-mem2reg"
translate_c_file examples/publications/2026/ATVA2026Spectral/C/vstte10_inv_inj.c examples/publications/2026/ATVA2026Spectral/C/vstte10_inv_inj.ll "-mem2reg"
translate_c_file examples/publications/2026/ATVA2026Spectral/C/vstte10_sum_max.c examples/publications/2026/ATVA2026Spectral/C/vstte10_sum_max.ll "-mem2reg"

# C++ Tests
echo "=============================================="
echo " Translating C++ Examples"
echo "=============================================="
translate_cpp_file examples/publications/2026/ATVA2026Spectral/Cpp/cantor.cpp examples/publications/2026/ATVA2026Spectral/Cpp/cantor.ll "-mem2reg"
translate_cpp_file examples/publications/2026/ATVA2026Spectral/Cpp/date.cpp examples/publications/2026/ATVA2026Spectral/Cpp/date.ll "-mem2reg"
translate_cpp_file examples/publications/2026/ATVA2026Spectral/Cpp/fibonacci.cpp examples/publications/2026/ATVA2026Spectral/Cpp/fibonacci.ll "-mem2reg"
translate_cpp_file examples/publications/2026/ATVA2026Spectral/Cpp/sort.cpp examples/publications/2026/ATVA2026Spectral/Cpp/sort.ll "-mem2reg"
translate_cpp_file examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_inv_inj.cpp examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_inv_inj.ll "-mem2reg"
translate_cpp_file examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_sum_max.cpp examples/publications/2026/ATVA2026Spectral/Cpp/vstte10_sum_max.ll "-mem2reg"

# Swift Tests
echo "=============================================="
echo " Translating Swift Examples"
echo "=============================================="
translate_swift_file examples/publications/2026/ATVA2026Spectral/Swift/cantor.swift examples/publications/2026/ATVA2026Spectral/Swift/cantor.ll "-mem2reg"
translate_swift_file examples/publications/2026/ATVA2026Spectral/Swift/date.swift examples/publications/2026/ATVA2026Spectral/Swift/date.ll "-mem2reg"
translate_swift_file examples/publications/2026/ATVA2026Spectral/Swift/fibonacci.swift examples/publications/2026/ATVA2026Spectral/Swift/fibonacci.ll "-mem2reg"

rm -r $W_DIR
