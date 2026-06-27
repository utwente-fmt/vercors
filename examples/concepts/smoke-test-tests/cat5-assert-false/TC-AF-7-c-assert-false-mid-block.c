//:: cases TC_AF_7_CAssertFalseMidBlock
//:: tools silicon
//:: verdict Fail

// assert false mid-block fails (assertFailed) and triggers block cutoff, so code
// after it (the if (x > 0)) is not instrumented for dead-code checks.
void f(int x) {
    x = x + 1;
    // assertFailed — block cutoff starts here
    //@ assert false;
    if (x > 0) {           // not instrumented (block cutoff)
        x = x - 1;
    }
    x = x + 2;
}
