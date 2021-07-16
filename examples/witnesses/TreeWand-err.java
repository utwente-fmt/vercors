// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TreeWandErr
//:: tools
//:: options --inline
//:: verdict Fail
//:: suite slow

// Example disabled because it must be rewritten.

/*
    This file demonstrates how a magic wand can be used to prove
    that the deletion of a node from a binary search tree is sound.
    
    vct --chalice --inline TreeWand.java

    The expected result is Pass.

*/

class Tree {
    public int data;
    public Tree left;
    public Tree right;
  
  /*@ public resource state()=Perm(data,100)**
      Perm(left,100)**Perm(right,100)**left->state()**right->state();
  @*/

  /*
    The definition of the contents of a list uses mutually recursive functions
    contents and tolist. The definitions are fairly complicated. The ensures
    clauses are extensive because they help Chalice proves facts faster.
    Moreover, the tolist funciton cannot be simplified by defining it as its
    third ensures clause becauses this causes the termination check to fail.
   */

  /*@
    requires t->state();
    ensures  t!=null ==> \result.length > 0;
    ensures  t==null ==> \result == seq<int>{};
    public seq<int> contents(Tree t)=
        (t==null)?
            seq<int>{}
        :
            \unfolding t.state() \in
                this.contents(t.left) + seq<int>{t.data} + this.contents(t.right);
  @*/

  /*@
    public resource contains(Tree t,seq<int>L)=t->state() ** L == contents(t);

    public boolean sorted_list(seq<int> s)=
      (\forall int i ; 1 < i && i < |s| ; s[i-1] <= s[i] );
      
    requires t->state();
    public boolean sorted(Tree t)=sorted_list(contents(t));
  @*/

    //@ requires top!=null ** top.state();
    //@ ensures  contains(\result,tail(\old(contents(top))));
    //@ ensures  \old(sorted(top)) ==> sorted(\result);
    public Tree del_min(Tree top) {
        //@ seq<int> orig_contents=contents(top);
        //@ seq<int> target_contents=tail(contents(top));
        //@ unfold top.state();
        if (top.left == null) {
            return top.right;
        } else {
            Tree cur, left;
            cur = top;
            left = top.left;
            //@ seq<int> cur_contents=orig_contents;
            //@ assert cur_contents == contents(left) + seq<int>{top.data} + contents(top.right);
            //@ unfold left.state();
      /*@
      loop_invariant Perm(cur.left,100) ** Perm(cur.data,100) ** Perm(cur.right,100);
      loop_invariant cur.left==left ** cur.right->state() ;
      loop_invariant Perm(left.left,100) ** Perm(left.data,100) ** Perm(left.right,100);
      loop_invariant left.left->state() ** left.right->state();
      loop_invariant cur_contents == (contents(left.left) + seq<int>{left.data} + contents(left.right))
                                      + seq<int>{cur.data} + contents(cur.right);
      loop_invariant wand:(contains(cur,tail(cur_contents)) -* contains(top,target_contents)); @*/
            while (left.left != null) /*@ with {
        create {} wand:(contains(top,target_contents) -* contains(top,target_contents));#\label{proof 1}#
      } @*/ { /*@ Tree prev = cur;
            seq<int> prev_contents = cur_contents; */
                cur = left;
                left = cur.left;
        /*@
        unfold left.state();
        cur_contents = contents(left.left) + seq<int>{left.data} + contents(left.right);
        cur_contents = cur_contents + seq<int>{cur.data} + contents(cur.right);
        assert prev_contents.length > 0 ;
        assert cur_contents.length > 0 ;
        assert prev_contents == cur_contents + seq<int>{prev.data} + contents(prev.right);
        create  {#\label{proof 2 begin}#
          use    prev_contents.length > 0 ;
          use    cur_contents.length > 0 ;
          use    Perm(prev.left,100)**Perm(prev.data,100);
          use    Perm(prev.right,100)**prev.right->state();
          use    prev.left==cur;
          use    prev_contents == cur_contents + seq<int>{prev.data} + contents(prev.right);
          fold   prev.state();
          apply  wand:(contains(prev,tail(prev_contents)) -* contains(top,target_contents));#\label{apply 1}#
          qed    wand:(contains(cur,tail(cur_contents)) -* contains(top,target_contents));
        } #\label{proof 2 end}#
        @*/
            }
            cur.left = left.left;
            //@ fold cur.state();
            //@ assert contents(cur)==tail(cur_contents);
            //@ apply wand:(contains(cur,tail(cur_contents)) -* contains(top,target_contents));#\label{apply 2}#

            return top;
        }
    }
}

