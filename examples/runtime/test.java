class Test {
    private int x;
    private int y;

    public void run() {
        //might be changing permission
    }

    /*@
        requires Perm(this.x, write);
        ensures Perm(this.x, read);
     */
    public int advancedFunction() {
        //Block 1
        int oldX = x;
        int oldY = y;
        x = oldX + oldY + x + y;


        if (run()) {

        }else if(this.x == this.x) {

        }
        //Block 2
        run();

        //Block 3
        int diffX = oldX -x;
        int diffY = oldY -x;

        x = oldX + x;
        y = oldY + y;
        return y;
    }
}


