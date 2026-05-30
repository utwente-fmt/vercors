//:: cases TC_AF_7_CAssertFalseMidBlock
//:: tools silicon
//:: verdict Fail

// TC-AF-7 (C): assert false mid-block — intentional dead code marker.
// Block cutoff applies: code after assert false in the same block is NOT instrumented.
// The if (x > 0) after assert false is not checked for reachability.
// The assert false itself fails (assertFailed).
//
// Expected errors: 1 (assertFailed from assert false).
void f(int x) {
    x = x + 1;
    //@ assert false;      // assertFailed — block cutoff starts here
    if (x > 0) {           // not instrumented (block cutoff)
        x = x - 1;
    }
    x = x + 2;
}
