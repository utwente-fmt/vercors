class Test {


    private int x;
    private int y;
    private int[] z;

    //@ resource testResource(frac a) = Perm(this.x, a) ** Perm(this.y, 1);


    /*@
        requires Perm(this.x, 1);
        requires Perm(this.y, 1);
        requires (\forall int i; 0 <= i && i < z.length; (\forall int j; 0 < j && i < j; z[i] != z[j] && z[i] > z[j]));
     */
    public void run() {
        int b;
        for (int i = 0; i < 10; i++) {

        }
    }

}


