class Test {


    private int x;
    private int y;
    private int[] z;


    public boolean trial(){
        return true;
    }

    public void test2() {

    }


    /*@
        requires Perm(this.x, 1);
        requires Perm(this.y, 1);
        requires (\forall int i; 0 <= i && i < z.length; z[i] > 0);
     */
    public void run() {
        int b;
        if(trial()) {
            b = 1;
        }

        test2();
    }

}


