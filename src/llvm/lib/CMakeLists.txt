add_subdirectory(Origin)
add_subdirectory(Util)
add_subdirectory(Transform)
add_subdirectory(Passes)

add_library(Plugin OBJECT
            Plugin.cpp)
target_link_libraries(Plugin COL Transform Passes Origin Util)
target_include_directories(Plugin PRIVATE
    "${CMAKE_SOURCE_DIR}/include"
)
target_precompile_headers(Plugin REUSE_FROM COL)

