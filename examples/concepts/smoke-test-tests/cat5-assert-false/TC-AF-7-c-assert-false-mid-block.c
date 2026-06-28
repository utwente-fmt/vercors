//:: cases TC_AF_7_CAssertFalseMidBlock
//:: tools silicon
//:: verdict Fail

// Same as TC-AF-1 but via C.
void f(int x) {
    x = x + 1;
    // assertFailed — block cutoff starts here
    //@ assert false;
    if (x > 0) {           // not instrumented (block cutoff)
        x = x - 1;
    }
    x = x + 2;
}
