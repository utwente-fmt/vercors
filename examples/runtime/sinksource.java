class Source extends Thread {
    int i;

    //@ resource postJoin(frac p) = Perm(this.i, p);

    /*@
        requires Perm(this.i, 1);
     */
    public void run() {
        i = 42;
    }
}

class Sink extends Thread {
    Source source;

    //@ resource postJoin(frac p) = Perm(this.source.i, p);

    public Sink(Source source) {
        this.source = source;
    }

    /*@
        requires Perm(source, 1);
     */
    public void run() {
        try {
            //@ source.postJoin(1\2);
            source.join();
        }catch (Exception e) {
            System.err.println(e.toString());
        }
        System.out.print("Sink: ");
        System.out.println(source.i);
    }
}

class Main{

    public static void main(String[] args) {
        Source source = new Source();
        Sink sink = new Sink(source);

        try {
            source.start();
            sink.start();

            //@ source.postJoin(1\2);
            source.join();
            System.out.print("Main: ");
            System.out.println(source.i);
            //@ sink.postJoin(1\2);
            sink.join();
        } catch (Exception e) {
            System.err.println(e.toString());
        }

        source.i = 1988;
        System.out.println(source.i);
    }
}