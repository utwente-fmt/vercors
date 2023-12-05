class Test {

    private int[] z;

    /*@
        requires (\exists int i; 0 <= i && i < z.length; (\forall int j; 0 <= j && j < i; z[i] > 0 && z[i] <= z[j]));
     */
    public void run() {

    }

}


