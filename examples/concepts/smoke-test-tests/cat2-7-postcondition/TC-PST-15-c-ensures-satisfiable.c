//:: cases TC_PST_15_CEnsuresSatisfiable
//:: tools silicon
//:: verdict Pass

// Same as TC-PST-1 but via C.
/*@ ensures \result > 0; @*/
int f(int x) {
    return 1;
}
