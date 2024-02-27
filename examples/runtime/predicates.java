public class Main {

    //@ requires c != null ** c.state(0);
    //@ ensures c.state(2);
    void foo(Counter c) {
        c.increment(2);
    }

    public void execute() {
        Counter c = new Counter();
        //@ fold c.state(0);
        Counter c2 = new Counter();
        //@ fold c2.state(0);

        foo(c);
        foo(c2);
    }

    public void main() {
        Main m = new Main();
        m.execute();
    }
}


class Counter {
    int count;

    //@ resource state(int val) = Perm(count, write) ** count == val;

    //@ requires state(count);
    //@ ensures state(count);
    void increment(int n) {
        int z = count;
        //@ unfold state(count);
        count += n;
        //@ fold state(count);
    };
}