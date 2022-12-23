//:: cases CatchContinue
//:: tools silicon
//:: verdict Pass

class CatchContinue {
    //@ requires n >= 0;
    void m (int n) {
        int i = 0;
        int[] xs = new int[n];

        //@ loop_invariant 0 <= i && i <= n;
        //@ loop_invariant xs != null && xs.length == n;
        //@ loop_invariant (\forall* int j = 0 .. n; Perm(xs[j], write));
        //@ loop_invariant (\forall int j = 0 .. i; j % 2 == 0 ==> xs[j] == 0);
        while (i < n) {
            try {
                int currentI = i;
                i++;
                if (currentI % 2 != 0) {
                    throw new Exception();
                } 
                xs[currentI] = 0;
            } catch (Exception e) {
                continue;
            }
        }

        //@ assert (\forall int j = 0 .. n; j % 2 == 0 ==> xs[j] == 0);
    }
}

