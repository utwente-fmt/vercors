// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ListAppendASyncDef
//:: tools silicon
//:: verdict Pass


/**
  In this version, the structuring of the definitions leads to many
  (un)fold annotations. This is because the recursion used for state
  and list is one step out of sync. In the inline version this is solved
  by inlining list and in ListAppend it is solved by using a single list
  predicate.
*/

final class List {

  public int val;
  public List next;
  
  /*@
  resource state() =
    Perm(val,1) ** Perm(next,1) ** next->state();

  requires state();
  pure seq<int> contents()=\unfolding state() \in
    (next==null ? [val] : val :: next.contents());

  resource list(seq<int> c) = state() ** contents()==c;
  @*/

  /*@
  ensures list([v]);
  @*/
  public List(int v){
    val=v;
    next=null;
    //@ fold state();
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
    //@ unfold state();
    if (next==null) {
        next=l;
        //@ unfold next.list(L2);
    } else {
        //@ ghost seq<int> tmp = next.contents();
        //@ fold next.list(tmp);
        next.append_rec/*@ given { L1 = tmp, L2 = L2 } @*/(l);
        //@ unfold next.list(tmp+L2);
    }
    //@ fold state();
    //@ fold list(L1+L2);
  }
}