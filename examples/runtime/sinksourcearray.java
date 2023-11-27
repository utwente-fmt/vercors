class Source extends Thread {
    int[] i;

    //@ resource postJoin(frac p, frac p2, frac p3)
    //= Perm(this.i, p) ** Perm(this.i[0], p2) ** Perm(this.i[1], p2);

    /*@
        requires Perm(this.i, 1);
     */
    public void run() {
        i = new int[]{1,2};
    }
}

class Sink extends Thread {
    Source source;

    //@ resource postJoin(frac p, frac p2)
    // = Perm(this.source.i, p) ** Perm(this.source.i[0], p2);

    public Sink(Source source) {
        this.source = source;
    }

    /*@
        requires Perm(source, 1);
     */
    public void run() {
        try {
            //@ source.postJoin(1\2, 1, 0);
            source.join();
        }catch (Exception e) {
            System.err.println(e.toString());
        }
        source.i[0] = 3;
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

            //@ source.postJoin(1\2, 0, 1);
            source.join();
            System.out.print("Main: ");
            System.out.println(source.i);
            //@ sink.postJoin(1\2, 1);
            sink.join();
        } catch (Exception e) {
            System.err.println(e.toString());
        }

        source.i[1] = 4;
        System.out.println(source.i);
    }
}