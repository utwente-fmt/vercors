add_library(Transform STATIC
    BlockTransform.cpp
    LoopContractTransform.cpp
    SpecStatementTransform.cpp
    Transform.cpp
    # Instruction/
    Instruction/BinaryOpTransform.cpp
    Instruction/CastOpTransform.cpp
    Instruction/FuncletPadOpTransform.cpp
    Instruction/IntrinsicsTransform.cpp
    Instruction/MemoryOpTransform.cpp
    Instruction/OtherOpTransform.cpp
    Instruction/TermOpTransform.cpp
    Instruction/UnaryOpTransform.cpp
)
target_link_libraries(Transform COL Util Origin)
target_include_directories(Transform PRIVATE
    "${CMAKE_SOURCE_DIR}/include"
)
target_precompile_headers(Transform REUSE_FROM COL)

