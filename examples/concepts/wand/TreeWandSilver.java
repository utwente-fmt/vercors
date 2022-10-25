// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TreeWandSilver
// suite puptol
// tools silicon
/*
    This file demonstrates how a magic wand can be used to prove
    that the deletion of a node from a binary search tree is sound.
    
    vct --silver=silicon TreeWandSilver.java

    The expected result is Pass.
*/

/*@
    requires t->state();
    pure seq<int> tolist(Tree t)=(t==null)?seq<int>{}:
        \unfolding t.state() \in tolist(t.left) + seq<int>{t.data} + tolist(t.right);

    requires t->state();
    pure boolean sorted(Tree t)=sorted_list(tolist(t));

    pure boolean sorted_list(seq<int> s)=
      (\forall int i ; 1 < i && i < |s| ; s[i-1] <= s[i] );
    @*/

final class Tree {
  public int data;
  public Tree left;
  public Tree right;
  
  /*@ resource state()=Perm(data,write)**
      Perm(left,write)**Perm(right,write)**left->state()**right->state();
  @*/

  /*@
    inline resource state_contains(seq<int> L)=this.state() ** tolist(this)==L;
        
    inline resource contains(Tree t,seq<int>L)=t->state() ** L == tolist(t);
  @*/

  //@ requires top!=null ** top.state();
  //@ ensures  contains(\result,\old(tolist(top)).tail);
  //@ ensures  \old(sorted(top)) ==> sorted(\result);
  public Tree del_min(Tree top){
    //@ ghost seq<int> orig_contents=tolist(top);
    //@ ghost seq<int> target_contents=(tolist(top)).tail;
    //@ unfold top.state();
    if (top.left == null) {
      //@ assert orig_contents == tolist(top.left) + seq<int>{top.data} + tolist(top.right);
      //@ assert tolist(top.left) == seq<int>{};
      return top.right;
    } else {
      Tree cur, left;
      cur = top;
      left = top.left;
      //@ ghost seq<int> cur_contents=orig_contents;
      //@ assert cur_contents == tolist(left) + seq<int>{top.data} + tolist(top.right);
      //@ unfold left.state();
      //@ package top.state_contains(target_contents) -* top.state_contains(target_contents) {}

      /*@
      loop_invariant Perm(cur.left,write) ** Perm(cur.data,write) ** Perm(cur.right,write);
      loop_invariant cur.left==left ** cur.right->state() ;
      loop_invariant Perm(left.left,write) ** Perm(left.data,write) ** Perm(left.right,write);
      loop_invariant left.left->state() ** left.right->state();
      loop_invariant cur_contents == (tolist(left.left) + seq<int>{left.data} + tolist(left.right))
                                      + seq<int>{cur.data} + tolist(cur.right);
      loop_invariant (cur.state_contains((cur_contents).tail) -* top.state_contains(target_contents));
      @*/
      while (left.left != null)
      {
        //@ ghost Tree prev = cur;
        //@ ghost seq<int> prev_contents = cur_contents;
        cur = left;
        left = cur.left;
        /*@
        unfold left.state();
        ghost cur_contents = tolist(left.left) + seq<int>{left.data} + tolist(left.right);
        ghost cur_contents = cur_contents + seq<int>{cur.data} + tolist(cur.right);
        assert |prev_contents| > 0 ;
        assert |cur_contents| > 0 ;
        assert prev_contents == cur_contents + seq<int>{prev.data} + tolist(prev.right);
        package cur.state_contains((cur_contents).tail) -* top.state_contains(target_contents) {}
        @*/
      }
      cur.left = left.right;
      //@ fold cur.state();
      //@ assert tolist(cur)==(cur_contents).tail;
      //@ assert cur.state_contains(tolist(cur));
      //@ assert cur.state_contains((cur_contents).tail);
      //@ apply (cur.state_contains((cur_contents).tail) -* top.state_contains(target_contents));
      return top;
    }
  }
}

