class C{
private int z;
private int r;
}

class Test{
    private C[] x;
    private int y;

    public Test() {

    }


    public void run() {

    }
    /*@
        requires Perm(x, 1);
        requires Perm(y, 1\2);
        requires Perm(x, write);
        requires Perm(y, read);
     */
    public int sum () {
        int b = y;

        int z = b + y;
        y = z;

        return y;
    }
}


class Thread {


    public void start();
    public void join();
    public void run();
}