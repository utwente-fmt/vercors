package java.lang;

public class String {
    //@ pure seq<int> data();

    /*@
    ghost
    ensures data() == data;
    String(seq<int> data) {
        //@ assume false;
    }
    */

    /*@
    ensures
    (\forall String p; p != null;
    (\forall String q; q != null;
        (p.intern() == q.intern()) == (p.data() == q.data())));
    */
    public /*@ pure */ String intern() {
        //@ assume false;
    }
}