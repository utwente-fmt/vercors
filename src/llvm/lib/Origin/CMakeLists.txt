add_library(Origin STATIC
    OriginProvider.cpp
    ShortPositionDeriver.cpp
    ContextDeriver.cpp
    PreferredNameDeriver.cpp)

target_link_libraries(Origin COL)
target_include_directories(Origin PRIVATE
    "${CMAKE_SOURCE_DIR}/include"
    "${PROTO_BINARY_DIR}"
)
