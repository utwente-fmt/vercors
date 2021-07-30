//:: cases ControlFlowCatchReturn
//:: tools silicon
//:: verdict Pass

class ControlFlowCatchReturn {
    //@ context_everywhere xs != null ** Perm(xs[*], write);
    //@ ensures \result != null ==> \result == xs;
    //@ ensures \result != null ==> (\forall int j = 0 .. \result.length; xs[j] == 1);
    //@ ensures \result == null ==> (\exists int j = 0 .. xs.length; xs[j] == 0);
    int[]  m(int[] xs) {
        int i = 0;

        //@ loop_invariant 0 <= i && i <= xs.length;
        //@ loop_invariant (\forall int j = 0 .. i; xs[j] == 1);
        //@ loop_invariant (\forall int j = i .. xs.length; xs[j] == \old(xs[j]));
        while (i < xs.length) {
            try {
                if (xs[i] != 0) {
                    xs[i] = xs[i] / xs[i];
                } else {
                    throw new ArithmeticException();
                }
            } catch (ArithmeticException e) {
                return null;
            }
            i++;
        }

        return xs;
    }
}





