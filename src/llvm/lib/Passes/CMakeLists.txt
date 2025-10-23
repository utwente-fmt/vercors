add_library(Passes STATIC
    # Function/
    Function/ExprWrapperMapper.cpp
    Function/FunctionBodyTransformer.cpp
    Function/FunctionContractDeclarer.cpp
    Function/FunctionDeclarer.cpp
    Function/PallasFunctionContractDeclarerPass.cpp
    Function/PureAssigner.cpp
    # Module/
    Module/GlobalVariableDeclarer.cpp
    Module/ModuleSpecCollector.cpp
    Module/ProtobufPrinter.cpp
    Module/RootContainer.cpp
)

target_link_libraries(Passes COL)
target_include_directories(Passes PRIVATE
    "${CMAKE_SOURCE_DIR}/include"
    "${PROTO_BINARY_DIR}"
)

