add_library(IRSpec STATIC
    PallasIRSpec.cpp
    PallasSpecDecoding.cpp
)
target_link_libraries(IRSpec Util)
target_include_directories(IRSpec PRIVATE
    "${CMAKE_SOURCE_DIR}/include"
)
target_precompile_headers(IRSpec REUSE_FROM COL)

