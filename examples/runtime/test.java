class Source extends Thread {
    int[] a;

    /*@
        requires Perm(this.a, 1);
        requires (\forall* int i; 0 <= i && i < a.length; Perm(this.a[i], 1));
     */
    public void run() {
        this.a = new int[2];
    }

    public void join(){

    }

    public void start() {

    }

}
class Main {

    public void main() {
        Source source = new Source();
        source.start();
        //@ source.postJoin(1\2);
        source.join();
        source.a[1] = 4;
    }
}

class Thread {

}
