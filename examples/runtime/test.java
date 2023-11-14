class C extends Runnable {
    public int z;
    public int r;

    public void run() {

    }
}

class Test {
    private C x;
    private int y;

    public Test() {

    }


    public void run() {

    }

    /*@
        requires Perm(x.z, 1);
        requires Perm(x.r, 1\3);
        requires Perm(y, 1\2);
        ensures Perm(x, write);
        ensures Perm(y, read);
     */
    public int sum() {

        return y;

    }
}


class Thread {

    public Thread(Runnable r) {

    }

    public void start();

    public void join();
}

interface Runnable {
    public void run();
}