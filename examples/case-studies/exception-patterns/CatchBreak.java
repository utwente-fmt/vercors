//:: cases CatchBreak
//:: tools silicon
//:: verdict Pass

class CatchBreak {
    //@ context_everywhere xs != null ** Perm(xs[*], write);
    void m (int[] xs) {
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
                break;
            }
            i++;
        }

        if (i == xs.length) {
            //@ assert (\forall int j = 0 .. xs.length; xs[j] == 1);
        } else {
            //@ assert i != xs.length ==> (\exists int j = 0 .. xs.length; xs[j] == 0);
        }
    }
}


