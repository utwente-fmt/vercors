//:: cases FinallyContinue
//:: tools silicon
//:: verdict Pass

class FinallyContinue {
    //@ requires n >= 0;
    void  m(int n) {
        int i = 0;
        boolean[] xs = new boolean[n];

        //@ loop_invariant 0 <= i && i <= n;
        //@ loop_invariant xs != null ** xs.length == n;
        //@ loop_invariant (\forall* int j = 0 .. n; Perm(xs[j], write));
        //@ loop_invariant (\forall int j = 0 .. i; xs[j]);
        while (i < n) {
            try {
                try {
                    xs[i] = true;
                    throw new Exception();
                    //@ assert false;
                } finally {
                    i++;
                    continue;
                    //@ assert false;
                }
            } catch (Exception e) {
                //@ assert false;
            }
        }

        //@ assert (\forall int j = 0 .. n; xs[j]);
    }
}






