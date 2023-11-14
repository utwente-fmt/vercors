class B {
    public int p;
}

class C extends Runnable {
    public B z;
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
        requires Perm(x.z.p, 1);
        ensures Perm(x.z.p, 1);
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