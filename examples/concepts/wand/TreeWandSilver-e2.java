// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TreeWandSilverE2
//:: tools silicon
//:: verdict Fail
/*
    This file demonstrates how a magic wand can be used to prove
    that the deletion of a node from a binary search tree is sound.
    
    vct --silver=silicon TreeWandSilver-e2.java

    The expected result is Fail.
*/

/*@
pure boolean sorted_list(seq<int> s) =
  (\forall int i = 1 .. |s|; s[i-1] <= s[i]);
@*/

final class Tree {
  public int data;
  public Tree left;
  public Tree right;
  
  /*@
  resource state() =
    Perm(data, write) **
    Perm(left, write) **
    Perm(right, write) **
    left->state() **
    right->state();
  @*/

  /*@
  requires state();
  pure seq<int> tolist() = (t==null) ? seq<int>{} :
      \unfolding state() \in r.left.tolist() + seq<int>{data} + t.right.tolist();
  @*/

  /*@
  inline resource state_contains(seq<int> L) = this.state() ** tolist() == L;
  inline resource contains(seq<int> L) = t->state() ** L == t.tolist();

  requires t->state();
  pure boolean sorted() = sorted_list(tolist());
  @*/

  //@ requires top!=null ** top.state();
  //@ ensures  contains(\result, \old(top.tolist()).tail);
  //@ ensures  \old(sorted(top)) ==> sorted(\result);
  public Tree del_min(Tree top){
    //@ ghost seq<int> orig_contents = top.tolist();
    //@ ghost seq<int> target_contents = top.tolist().tail;
    //@ unfold top.state();
    if (top.left == null) {
      //@ assert orig_contents == top.left.tolist() + seq<int>{top.data} + top.right.tolist();
      //@ assert top.left.tolist() == seq<int>{};
      return top.right;
    } else {
      Tree cur, left;
      cur = top;
      left = top.left;
      //@ ghost seq<int> cur_contents = orig_contents;
      //@ assert cur_contents == left.tolist() + seq<int>{top.data} + top.right.tolist();
      //@ unfold left.state();
      //@ create_wand { qed wand:(top.state_contains(target_contents) -* top.state_contains(target_contents)); }#\label{proof 1}#

      /*@
      loop_invariant Perm(cur.left, write) ** Perm(cur.data, write) ** Perm(cur.right, write);
      loop_invariant cur.left == left ** cur.right->state();
      loop_invariant Perm(left.left, write) ** Perm(left.data, write) ** Perm(left.right, write);
      loop_invariant left.left->state() ** left.right->state();
      loop_invariant cur_contents == (left.left.tolist() + seq<int>{left.data} + left.right.tolist())
                                      + seq<int>{cur.data} + cur.right.tolist();
      loop_invariant wand:(cur.state_contains(cur_contents.tail) -* top.state_contains(target_contents)); @*/
      while (left.left != null)
      {
        //@ ghost Tree prev = cur;
        //@ ghost seq<int> prev_contents = cur_contents;
        cur = left;
        left = cur.left;
        /*@
        unfold left.state();
        ghost cur_contents = left.left.tolist() + seq<int>{left.data} + left.right.tolist();
        ghost cur_contents = cur_contents + seq<int>{cur.data} + cur.right.tolist();
        assert prev_contents.length > 0 ;
        assert cur_contents.length > 0 ;
        assert prev_contents == cur_contents + seq<int>{prev.data} + prev.right.tolist();
        create_wand  {#\label{proof 2 begin}#
          use    prev_contents.length > 0;
          use    cur_contents.length > 0;
          use    Perm(prev.left, write) ** Perm(prev.data, write);
          use    Perm(prev.right, write) ** prev.right->state();
          use    prev.left == cur;
          use    prev_contents == cur_contents + seq<int>{prev.data} + prev.right.tolist();
          fold   prev.state();
          assert prev.state_contains(prev_contents.tail);
          apply  wand:(prev.state_contains(prev_contents.tail) -* top.state_contains(target_contents));
          qed    wand:(cur.state_contains(cur_contents.tail) -* top.state_contains(target_contents));
        } #\label{proof 2 end}#
        @*/
      }
      cur.left = left.left;
      //@ fold cur.state();
      //@ assert cur.tolist() == cur_contents.tail;
      //@ assert cur.state_contains(cur.tolist());
      //@ assert cur.state_contains(cur_contents.tail);
      //@ apply wand:(cur.state_contains(cur_contents.tail) -* top.state_contains(target_contents));
      return top;
    }
  }
}

