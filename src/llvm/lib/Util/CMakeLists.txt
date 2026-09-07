add_library(Util STATIC
    BlockUtils.cpp
    Exceptions.cpp
    PallasDIMapping.cpp
    PallasMD.cpp
    PallasWrapperUtils.cpp
)
target_link_libraries(Util Transform COL Origin IRSpec)
target_include_directories(Util PRIVATE
    "${CMAKE_SOURCE_DIR}/include"
)
target_precompile_headers(Util REUSE_FROM COL)

