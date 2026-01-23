add_library(Origin STATIC
    OriginProvider.cpp
    ShortPositionDeriver.cpp
    ContextDeriver.cpp
    PreferredNameDeriver.cpp)

target_link_libraries(Origin COL IRSpec)
target_include_directories(Origin PRIVATE
    "${CMAKE_SOURCE_DIR}/include"
)
target_precompile_headers(Origin REUSE_FROM COL)

