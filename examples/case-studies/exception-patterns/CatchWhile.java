//:: cases CatchWhile
//:: tools silicon
//:: verdict Pass

class CatchWhile {
    boolean randomBoolean();

    //@ requires n >= 0;
    void m(int n) {
        boolean[] xs = new boolean[n];
        boolean throwB = randomBoolean();

        try {
            if (throwB) {
                throw new Exception();
            }
        } catch (Exception e) {
            int i = 0;
            //@ loop_invariant 0 <= i && i <= n;
            //@ loop_invariant xs != null ** xs.length == n;
            //@ loop_invariant (\forall* int j = 0 .. n; Perm(xs[j], write));
            //@ loop_invariant (\forall int j = 0 .. i; xs[j]);
            while (i < n) {
                xs[i] = true;
                i++;
            }
        }

        //@ assert xs != null ** xs.length == n;
        //@ assert (\forall* int j = 0 .. n; Perm(xs[j], write));

        if (throwB) {
            //@ assert (\forall int j = 0 .. n; xs[j]);
        } else {
            //@ assert (\forall int j = 0 .. n; !xs[j]);
        }
    }
}



