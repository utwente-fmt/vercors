class Source extends Thread {
    int i;

    /*@
        requires Perm(this.i, 1);
        ensures Perm(this.i, 1);
     */
    public void run() {
        i = 42;
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
    */
    public void run() {
        //@ source.postJoin(1\2);
        source.join();
    }

    public void join(){}
    public void start(){}
}

class Main{

    public void main() {
        Source source = new Source();
        Sink sink = new Sink();
        sink.setSource(source);

        source.start();
        sink.start();
        //@ source.postJoin(1\2);
        source.join();
        //@ sink.postJoin(1\2);
        sink.join();
        source.i = 1988;
    }
}

class Thread{

}