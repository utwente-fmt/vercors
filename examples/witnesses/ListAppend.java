// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ListAppend
//:: tools silicon


/**
 * This example shows how to use the given keyword to pass
 * verification level arguments and also how to use a magic
 * wand to prove the iterative implementation of list append
 * correct.
 * <p>
 * The command line to verify with the VerCors Tool is:
 * <p>
 * vct --silicon ListAppend.java
 * <p>
 * The expected result is Pass.
 */

final class List {

    public int val;
    public List next;
  
  /*@
    public final resource list(seq<int> c)=Perm(val,1)**Perm(next,1)**
      ((next==null)?(c==seq<int>{val}):(|c| > 0 ** head(c)==val ** next.list(tail(c))));
  @*/

    /*@
      ensures list(seq<int>{v});
    @*/
    public List(int v) {
        val = v;
        next = null;
        //@ fold list(seq<int>{v});
    }

    /*@
      given    seq<int> L1;
      given    seq<int> L2;
      requires this.list(L1);
      requires l!=null ** l.list(L2);
      ensures  this.list(L1+L2);
    @*/
    public void append_rec(List l) {
        //@ unfold list(L1);
        if (next == null) {
            next = l;
            //@ fold list(L1+L2);
        } else {
            next.append_rec(l) /*@ with { L1 = tail(L1) ; L2 = L2 ; } @*/;
            //@ fold list(seq<int>{head(L1)}+(tail(L1)+L2));
            //@ assert |L1|>0;
            //@ assert seq<int>{head(L1)}+(tail(L1)+L2) == L1+L2;
        }
    }


    /*@
      given    seq<int> L;
      requires list(L);
     */
    public /*@ pure @*/ List get_next() {
        //@ unfold list(L);
        return next;
    }

    /*@
      given    seq<int> L1;
      given    seq<int> L2;
      requires this.list(L1);
      requires l!=null ** l.list(L2);
      ensures  this.list(L1+L2);
    @*/
    public void append_iter(List l) {
        List cursor = this;
        //@ ghost seq<int> prefix=seq<int>{};
        //@ ghost seq<int> suffix=L1;

        //@ create { qed this.list(L1+L2) -* this.list(L1+L2); }

        //@ loop_invariant cursor!=null;
        //@ loop_invariant cursor.list(suffix);
        //@ loop_invariant prefix+suffix==L1;
        //@ loop_invariant l!=null ** l.list(L2);
        //@ loop_invariant cursor.list(suffix+L2) -* this.list(L1+L2);
        while (cursor.get_next()/*@ with { L = suffix; } */ != null) {
            //@ ghost List tmp=cursor;
            //@ ghost seq<int> tmp_suffix=suffix;
            //@ unfold cursor.list(suffix);
            //@ ghost prefix=prefix+seq<int>{cursor.val};
            //@ ghost suffix=tail(suffix);
            cursor = cursor.next;
        /*@
            create {
                use    Perm(tmp.val,1);
                use    Perm(tmp.next,1);
                use    tmp.next==cursor;
                use    cursor!=null;
                use    tmp_suffix==seq<int>{tmp.val}+suffix;
                use    tmp.list(tmp_suffix+L2) -* this.list(L1+L2);
                fold   tmp.list(tmp_suffix+L2);
                apply  tmp.list(tmp_suffix+L2) -* this.list(L1+L2);
                qed    cursor.list(suffix+L2) -* this.list(L1+L2);
            }
        @*/
        }
        //@ unfold cursor.list(suffix);
        cursor.next = l;
        //@ fold cursor.list(suffix+L2);
        //@ apply cursor.list(suffix+L2) -* this.list(L1+L2);
    }

}

