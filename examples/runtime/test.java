public class PredicateTest {

    //@ requires c != null ** c.state(0);
    //@ ensures c.state(2);
    void foo(Counter c) {
        c.increment(2);
    }


    public void test() {
        Counter c = new Counter();
        //@ fold c.state(0);
        Counter c2 = new Counter();
        //@ fold c2.state(0);

        foo(c);
        foo(c2);
    }
}


class Counter {
    int count;

    //@ resource state(int val) = Perm(count, write) ** count == val;

    //@ requires state(count);
    //@ ensures state(\old(count) + n);
    void increment(int n) {
        //@ unfold state(count);
        count += n;
        //@ fold state(count);
    };
}