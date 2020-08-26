// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases KeYAbruptTerminationChallenge
//:: tools silicon
//:: verdict Pass

// This is an adaptation of a file from the examples directory of key.
// Original file path: KEY_ROOT/key/key.ui/examples/standard_key/challenges/jacobsEtAl/abruptTermination/AbruptTermination.java
// It has been adapted to use the vercors syntax for specifications.

class AbruptTermination {
    int[] ia;

    //@ context_everywhere Perm(ia, read) ** ia != null;
    //@ context_everywhere (\forall* int i; 0 <= i && i < ia.length; Perm(ia[i], write));
    //@ ensures ia.length == \old(ia.length); // Extra contract needed for VerCors
    // ensure i is the first position with negative value
    /*@ ensures (\forall int i; 0 <= i && i < ia.length;
                    (\old(ia[i]) < 0 && (\forall int j; 0 <= j && j < i; \old(ia[j]) >= 0))
                        ? (ia[i] == -\old(ia[i]))
                        : (ia[i] == \old(ia[i]))
                );
    @*/
    void negatefirst() {
        /*@
            loop_invariant 0 <= i && i <= ia.length;
            loop_invariant ia == \old(ia); // Extra invariant needed. See: https://github.com/utwente-fmt/vercors/issues/511
            loop_invariant (\forall int j; 0<=j && j<i; ia[j] >= 0 && ia[j] == \old(ia[j]));
            loop_invariant (\forall int j; i<=j && j<ia.length; ia[j] == \old(ia[j])); // Extra invariant needed
         */
        for (int i = 0 ; i < ia.length; i++) {
            //@ assert ia == \old(ia);
            if (ia[i] < 0) {
                ia[i] = -ia[i] ;
                break ;
            }
        }
        //@ assert ia == \old(ia);
    }
}
