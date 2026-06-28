//:: cases TC_PST_16_CEnsuresUnsat
//:: tools silicon
//:: verdict Fail

// Same as TC-PST-2 but via C.
/*@ ensures \result > 0 && \result < 0; @*/
int f(int x) { return 1; }
