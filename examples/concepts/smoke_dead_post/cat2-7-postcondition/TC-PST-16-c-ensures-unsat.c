//:: cases TC_PST_16_CEnsuresUnsat
//:: tools silicon
//:: verdict Fail

// TC-PST-16 (C): \result > 0 && \result < 0 is always false — no value satisfies both.
// requires false makes normal verification vacuous; only postUnsatisfiable fires.
// Verifies the postSat checker works correctly through the C frontend.
/*@ requires false;
    ensures \result > 0 && \result < 0; @*/
int f(int x) { return 1; }
