class Test {

    private int[] z;

    /*@
        requires (\exists int i, int p; 0 <= i && i < z.length&& p > -19 && p < i; (\forall int j; 0 <= j && j < i ; z[i] > 0 && z[i] <= z[j] && z[p] != z[i]));
     */
    public void run() {

    }

}


