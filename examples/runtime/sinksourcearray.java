class Source extends Thread {
    int[] i;

    public void createI() {
        this.i = new int[2];
    }

    /*@
        requires Perm(this.i, 1);
        requires (\forall* int j; 0 <= j && j < i.length; Perm(i[j], write));
        ensures Perm(this.i, 1);
        ensures (\forall* int j; 0 <= j && j < i.length; Perm(i[j], write));
     */
    public void run() {
        i[0] = 42;
        i[1] = 43;
    }


    public void join(){}
    public void start(){}
}

class Sink extends Thread {
    Source source;

    public void setSource(Source source) {
        this.source = source;
    }

    /*@
       requires Perm(source, 1);
       ensures Perm(source, 1);
       ensures Perm(source.i, 1/2);
       ensures (\forall* int j; 0 <= j && j < source.i.length ==> Perm(source.i[j], 1/2));
    */
    public void run() {
        //@ source.postJoin(1\2);
        source.join();

        source.i[0] = 33;   //should throw an error
    }

    public void join(){}
    public void start(){}
}

class Main{

    public void main() {
        Source source = new Source();
        Sink sink = new Sink();
        sink.setSource(source);
        source.createI();

        source.start();
        sink.start();
        //@ source.postJoin(1/2);
        source.join();
        //@ sink.postJoin(1);
        sink.join();
        source.i[0] = 1988;
    }
}

class Thread{

}