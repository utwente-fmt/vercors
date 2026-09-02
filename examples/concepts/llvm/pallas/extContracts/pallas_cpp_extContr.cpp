// Test that external contracts that are defined in a separate file are loaded
// correctly. Needs to be transformed with ´pallas_cpp_extContrImpl´ as a
// provided contract-file.

/*@
declare using namespace pallasSpec;
@*/

// The contract for this is defined externally
int anAmazingExternalFunction(int a, int b);

/*@
requires n >= 0;
ensures _result<int>() >= 0;
@*/
int foo(int n) {
    int oldN = n;
    oldN += 1;
    oldN -= 1;
    if (n < 42) {
        return n;
    }
    int res = 0;
    /*@
    loop_invariant 0 <= i && i <= n + 1;
    loop_invariant res >= 0 ;
    @*/
    for (int i = 0; i <= n; i++) {
        res += i;
    }

    /*@
    assert oldN == n;
    @*/

    res += anAmazingExternalFunction(res, 1);
    return res;
}

/*@ wrapper location; @*/