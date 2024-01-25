class Source extends Thread {
    int[] a;

    //@ resource state() = Perm(this.a, 1) **(\forall* int i; 0 < i && i < a.length; Perm(a[i], write));


    /*@
        requires this.state();
     */
    public void run() {

    }

    public void start() {

    }

    public void join() {

    }
}

class Main {

    public void main() {
        Source source = new Source();
        source.start();

        //@ source.postJoin(1);
        source.join();
    }
}

class Thread{

}