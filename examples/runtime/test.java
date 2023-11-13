class C extends Runnable {
    public int z;
    public int r;

    public void run() {

    }
}

class Test {
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
    public int sum() {

//            check y permission
        int b = y;

        int z = b + y;


        C c = new C();  //Has all the permissions


        int a = c.z;


//            Check nothing
        Thread t = new Thread(c);


//            Check Nothing
//            Remove permissions
        t.start();


//            Check permissions for c.r and g.z
        int g = c.r + c.z;
        y = z;

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