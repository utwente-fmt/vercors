// This contract should fail when used with pallas_cpp_extContr.cpp
/*@
contract for external _Z25anAmazingExternalFunctionii;
args int a, int b;
returns int;
requires a >= 42 && b >= 0;
ensures _result<int>() >= 0;
@*/