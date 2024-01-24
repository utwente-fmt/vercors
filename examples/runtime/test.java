public class PredicateTest {

    //@ requires c != null ** c.state();
    //@ ensures c.state();
    void foo(Counter c) {
        c.increment(2);
    }


    public void test() {
        Counter c = new Counter();
        //@ fold c.state();
        Counter c2 = new Counter();
        //@ fold c2.state();

        foo(c);
        foo(c2);
    }
}


class Counter {
    int[] a;

    //@ resource state() = (\forall* int i; 0 < i && i < a.length; Perm(a[i], write));

    //@ requires state();
    //@ ensures state();
    void increment(int n) {
        //@ unfold state();

        //@ fold state();
    };
}