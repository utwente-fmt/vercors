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
    public resource state()=
      Perm(val,1)**Perm(next,1)**next->state();

    requires state();
    public pure seq<int> contents()=\unfolding state() \in
        ((next==null)?(seq<int>{val})
                    :(seq<int>{val}+next.contents()));

    public resource list(seq<int> c)=state() ** contents()==c;
  @*/

  /*@
    ensures list(seq<int>{v});
  @*/
  public List(int v){
    val=v;
    next=null;
    //@ fold state();
    //@ fold list(seq<int>{v});
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
        next.append_rec(l) /*@ with { L1 = tmp ; L2 = L2 ; } @*/;
        //@ unfold next.list(tmp+L2);
    }
    //@ fold state();
    //@ fold list(L1+L2);
  }

}

