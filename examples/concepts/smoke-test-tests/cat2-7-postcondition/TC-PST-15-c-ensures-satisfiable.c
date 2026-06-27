//:: cases TC_PST_15_CEnsuresSatisfiable
//:: tools silicon
//:: verdict Pass

// TC-PST-15 (C): Postcondition \result > 0 is satisfiable on its own (e.g., result=1).
// Verifies the postSat checker works correctly through the C frontend.
/*@ ensures \result > 0; @*/
int f(int x) {
    return 1;
}
