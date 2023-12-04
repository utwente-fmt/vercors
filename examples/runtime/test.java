class Test {

    private int[] z;

    /*@
        requires (\exists int i, int j; 0 <= i && i < z.length && 0 <= j && j < i; z[i] > 0 && z[i] <= z[j]);
        requires z.length > 10 || (z.length < 5 && z[0] == 0);
     */
    public void run() {

    }

}


