package java.lang;

public class String {
    //@ pure seq<int> data();

    /*@
    ghost
    ensures \result != null ** \result.data() == data;
    pure static String of(seq<int> data);
    */

    /*@
    ghost
    ensures
    (\forall String p; p != null;
    (\forall String q; q != null;
        (p.intern() == q.intern()) == (p.data() == q.data())));
    pure String intern();
    */
}