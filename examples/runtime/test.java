public class PredicateTest {

    //@ requires c != null ** c.state(0);
    //@ ensures c.state(0);
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
    int[] a;

    //@ resource state(int j) = (\forall* int i; 0 < i && i < a.length; Perm(a[i], write) ** a[i] == j);

    //@ requires state(0);
    //@ ensures state(0);
    void increment(int n) {
    };
}