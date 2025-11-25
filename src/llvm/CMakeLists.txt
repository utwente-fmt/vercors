cmake_minimum_required(VERSION 3.12)
project(Pallas)

set(CMAKE_CXX_STANDARD 20)

# LLVM
find_package(LLVM 17 REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

link_directories(${LLVM_LIBRARY_DIRS})

# LLVM is normally built without RTTI. Be consistent with that.
if (NOT LLVM_ENABLE_RTTI)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
    set(protobuf_DISABLE_RTTI ON)
endif ()

llvm_map_components_to_libnames(LLVM_LIBS core support irreader passes analysis)
# end LLVM


set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Protobuf
set(protobuf_BUILD_TESTS OFF)
set(ABSL_PROPAGATE_CXX_STD ON)

add_subdirectory("${PROTOBUF_DIR}" "${CMAKE_CURRENT_BINARY_DIR}/protobuf")
include("${PROTOBUF_DIR}/cmake/protobuf-generate.cmake")

# enable extra warnings
if (MSVC)
    # warning level 4
    add_compile_options(/W4)
else()
    # additional warnings
    add_compile_options(-Wall -Wextra -Wpedantic -Wno-unused-parameter -Wno-misleading-indentation)
endif()

# COL
add_library(COL STATIC
"${SCALA_PB_DIR}/scalapb/scalapb.proto"
"${SERIALIZE_DIR}/vct/col/ast/BigDecimal.proto"
"${SERIALIZE_DIR}/vct/col/ast/BigInt.proto"
"${SERIALIZE_DIR}/vct/col/ast/BitString.proto"
"${SERIALIZE_DIR}/vct/col/ast/Blame.proto"
"${SERIALIZE_DIR}/vct/col/ast/ExpectedError.proto"
"${SERIALIZE_DIR}/vct/col/ast/Origin.proto"
"${SERIALIZE_DIR}/vct/col/ast/Ref.proto"
"${MEGACOL_DIR}/vct/col/ast/col.proto"
)
target_link_libraries(COL PUBLIC protobuf::libprotobuf)
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/generated")
set(PROTO_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/generated")
target_include_directories(COL PUBLIC "$<BUILD_INTERFACE:${PROTO_BINARY_DIR}>")
target_precompile_headers(COL PRIVATE "$<BUILD_INTERFACE:${PROTO_BINARY_DIR}>/vct/col/ast/col.pb.h")

protobuf_generate(
        TARGET COL
        TARGET_DIR "."
        IMPORT_DIRS "${SERIALIZE_DIR}" "${SCALA_PB_DIR}" "${MEGACOL_DIR}"
        PROTOC_OUT_DIR "${PROTO_BINARY_DIR}")


add_subdirectory(lib)

add_library(Pallas SHARED
    $<TARGET_OBJECTS:Plugin>
    $<TARGET_OBJECTS:Transform>
    $<TARGET_OBJECTS:Passes>
    $<TARGET_OBJECTS:Origin>
    $<TARGET_OBJECTS:Util>
    $<TARGET_OBJECTS:COL>
    $<TARGET_OBJECTS:protobuf::libprotobuf>
)
target_link_libraries(Pallas PUBLIC Plugin Transform Passes Origin Util)
