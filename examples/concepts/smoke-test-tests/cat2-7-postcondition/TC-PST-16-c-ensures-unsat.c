//:: cases TC_PST_16_CEnsuresUnsat
//:: tools silicon
//:: verdict Fail

// TC-PST-16 (C): \result > 0 && \result < 0 is always false — no value satisfies both.
// Triggers both a normal postcondition failure and postUnsatisfiable.
// Verifies the postSat checker works correctly through the C frontend.
/*@ ensures \result > 0 && \result < 0; @*/
int f(int x) { return 1; }
