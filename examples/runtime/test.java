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
        ensures Perm(x, write);
        ensures Perm(y, read);
     */
    public int sum() {

//            check y permission
        int b = y;

        int z = b + y;
        y = z + 6;

        C ditIsEenVariable = new C();  //Has all the permissions


        int a = ditIsEenVariable.z;


//            Check nothing
        Thread t = new Thread(ditIsEenVariable);


//            Check Nothing
//            Remove permissions
        t.start();


//            Check permissions for c.r and g.z
         ditIsEenVariable.r = a + ditIsEenVariable.z;
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