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

final class Tree {
    public int data;
    public Tree left;
    public Tree right;
  
  /*@ public resource state()=Perm(data,write)**
      Perm(left,write)**Perm(right,write)**left->state()**right->state();
  @*/

  /*@
    requires t->state();
    public static seq<int> tolist(Tree t)=(t==null)?seq<int>{}:
        \unfolding t.state() \in tolist(t.left) + seq<int>{t.data} + tolist(t.right);
    @*/

  /*@
    public inline resource state_contains(seq<int> L)=this.state() ** tolist(this)==L;
        
    public static inline resource contains(Tree t,seq<int>L)=t->state() ** L == tolist(t);

    public static boolean sorted_list(seq<int> s)=
      (\forall int i ; 1 < i && i < |s| ; s[i-1] <= s[i] );
      
    requires t->state();
    public static boolean sorted(Tree t)=sorted_list(tolist(t));
  @*/

    //@ requires top!=null ** top.state();
    //@ ensures  contains(\result,tail(\old(tolist(top))));
    //@ ensures  \old(sorted(top)) ==> sorted(\result);
    public Tree del_min(Tree top) {
        //@ seq<int> orig_contents=tolist(top);
        //@ seq<int> target_contents=tail(tolist(top));
        //@ unfold top.state();
        if (top.left == null) {
            //@ assert orig_contents == tolist(top.left) + seq<int>{top.data} + tolist(top.right);
            //@ assert tolist(top.left) == seq<int>{};
            return top.right;
        } else {
            Tree cur, left;
            cur = top;
            left = top.left;
            //@ seq<int> cur_contents=orig_contents;
            //@ assert cur_contents == tolist(left) + seq<int>{top.data} + tolist(top.right);
            //@ unfold left.state();
      /*@
      loop_invariant Perm(cur.left,write) ** Perm(cur.data,write) ** Perm(cur.right,write);
      loop_invariant cur.left==left ** cur.right->state() ;
      loop_invariant Perm(left.left,write) ** Perm(left.data,write) ** Perm(left.right,write);
      loop_invariant left.left->state() ** left.right->state();
      loop_invariant cur_contents == (tolist(left.left) + seq<int>{left.data} + tolist(left.right))
                                      + seq<int>{cur.data} + tolist(cur.right);
      loop_invariant cur.state_contains(tail(cur_contents)) -* top.state_contains(target_contents); @*/
            while (left.left != null) /*@ with {
        create { qed top.state_contains(target_contents) -* top.state_contains(target_contents); }#\label{proof 1}#
      } @*/ { /*@ Tree prev = cur;
            seq<int> prev_contents = cur_contents; */
                cur = left;
                left = cur.left;
        /*@
        unfold left.state();
        cur_contents = tolist(left.left) + seq<int>{left.data} + tolist(left.right);
        cur_contents = cur_contents + seq<int>{cur.data} + tolist(cur.right);
        assert prev_contents.length > 0 ;
        assert cur_contents.length > 0 ;
        assert prev_contents == cur_contents + seq<int>{prev.data} + tolist(prev.right);
        create  {#\label{proof 2 begin}#
          use    prev_contents.length > 0 ;
          use    cur_contents.length > 0 ;
          use    Perm(prev.left,write)**Perm(prev.data,write);
          use    Perm(prev.right,write)**prev.right->state();
          use    prev.left==cur;
          use    prev_contents == cur_contents + seq<int>{prev.data} + tolist(prev.right);
          use    prev.state_contains(tail(prev_contents)) -* top.state_contains(target_contents);
          fold   prev.state();
          assert prev.state_contains(tail(prev_contents));
          apply  prev.state_contains(tail(prev_contents)) -* top.state_contains(target_contents);
          qed    cur.state_contains(tail(cur_contents)) -* top.state_contains(target_contents);
        } #\label{proof 2 end}#
        @*/
            }
            cur.left = left.right;
            //@ fold cur.state();
            //@ assert tolist(cur)==tail(cur_contents);
            //@ assert cur.state_contains(tolist(cur));
            //@ assert cur.state_contains(tail(cur_contents));
            //@ apply  cur.state_contains(tail(cur_contents)) -* top.state_contains(target_contents);
            return top;
        }
    }
}

