public class PredicateTest {

    //@ requires c != null ** c.state(0,1);
    //@ ensures c.state(2,1);
    void foo(Counter c) {
        c.increment(2);
    }


    public void main(String[] args) {
        Counter c = new Counter();
        //@ fold c.state(0,1);
        Counter c2 = new Counter();
        //@ fold c2.state(0,1);

        foo(c);
        foo(c2);
    }
}


class Counter {
    int count;

    //@ resource state(int val, int bal) = Perm(count, write) ** count == val;

    //@ requires state(count, 1);
    //@ ensures state(\old(count) + n, 1);
    void increment(int n) {
        //@ unfold state(count ,1);
        count += n;
        //@ fold state(count,1);
    };
}

class String {

}