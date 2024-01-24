class Source extends Thread {
    int[] a;

    /*@
        requires Perm(this.a, 1);
        requires (\forall* int i; 0 <= i && i < a.length; Perm(a[i], write) ** (\forall* int j; 0 < j && j < a.length; a[i] == a[j]));
        ensures (\forall* int i; 0 <= i && i < a.length; Perm(a[i], write) ** (\forall* int j; 0 < j && j < a.length; a[i] == a[j]));
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

        //@ source.postJoin(1/2);
        source.join();
    }
}

class Thread{

}