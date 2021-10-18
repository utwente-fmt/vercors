// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ListAppend
//:: tools silicon


/**
  This example shows how to use the given keyword to pass
  verification level arguments and also how to use a magic
  wand to prove the iterative implementation of list append
  correct.
  
  The command line to verify with the VerCors Tool is:
  
  vct --silicon ListAppend.java
  
  The expected result is Pass.
*/

final class List {
  public int val;
  public List next;
  
  /*@
  resource list(seq<int> c) = Perm(val,1) ** Perm(next,1) **
    next==null ? c == [val] : |c| > 0 ** c.head == val ** next.list(c.tail);
  @*/

  /*@
    ensures list([v]);
  @*/
  public List(int v){
    val=v;
    next=null;
    //@ fold list([v]);
  }
  
  /*@
  given    seq<int> L1;
  given    seq<int> L2;
  requires this.list(L1);
  requires l!=null ** l.list(L2);
  ensures  this.list(L1+L2);
  @*/
  public void append_rec(List l){
    //@ unfold list(L1);
    if (next==null) {
      next=l;
      //@ fold list(L1+L2);
    } else {
      next.append_rec/*@ given { L1 = L1.tail, L2 = L2 } @*/(l);
      //@ fold list([L1.head] + (L1.tail+L2));
      //@ assert |L1|>0;
      //@ assert [L1.head] + (L1.tail + L2) == L1+L2;
    }
  }
  
  
  /*@
  given    seq<int> L;
  requires list(L);
  @*/
  public /*@ pure @*/ List get_next(){
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
  public void append_iter(List l){
    List cursor=this;
    //@ ghost seq<int> prefix = seq<int>{};
    //@ ghost seq<int> suffix = L1;

    //@ create_wand { qed this.list(L1+L2) -* this.list(L1+L2); }

    //@ loop_invariant cursor!=null;
    //@ loop_invariant cursor.list(suffix);
    //@ loop_invariant prefix+suffix==L1;
    //@ loop_invariant l!=null ** l.list(L2);
    //@ loop_invariant cursor.list(suffix+L2) -* this.list(L1+L2);
    while(cursor.get_next/*@ given { L = suffix } @*/() != null)
    {
        //@ ghost List tmp=cursor;
        //@ ghost seq<int> tmp_suffix = suffix;
        //@ unfold cursor.list(suffix);
        //@ ghost prefix = prefix + [cursor.val];
        //@ ghost suffix = suffix.tail;
        cursor=cursor.next;
        /*@ create_wand {
          use    Perm(tmp.val,1);
          use    Perm(tmp.next,1);
          use    tmp.next==cursor;
          use    cursor!=null;
          use    tmp_suffix==seq<int>{tmp.val}+suffix;
          use    tmp.list(tmp_suffix+L2) -* this.list(L1+L2);
          fold   tmp.list(tmp_suffix+L2);
          apply  tmp.list(tmp_suffix+L2) -* this.list(L1+L2);
          qed    cursor.list(suffix+L2) -* this.list(L1+L2);
        } @*/
    }
    //@ unfold cursor.list(suffix);
    cursor.next=l;
    //@ fold cursor.list(suffix+L2);
    //@ apply cursor.list(suffix+L2) -* this.list(L1+L2);
  }

}

