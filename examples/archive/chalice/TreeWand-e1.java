// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TreeWand-E1
//:: tools
//:: options --inline
//:: verdict Fail
//:: suite slow

// Example disabled because it must be rewritten.

/*
    This file demonstrates how a magic wand can be used to prove
    that the deletion of a node from a binary search tree is sound.
    
    vct --chalice --inline TreeWand-e1.java

    The expected result is Fail.

    Beware that this specification can take 5-10 minutes to check.
*/

class Tree {
  public int data;
  public Tree left;
  public Tree right;
  
  /*@ resource state()=Perm(data,100)**
      Perm(left,100)**Perm(right,100)**(left->state())**(right->state());
  @*/

  /*
    The definition of the contents of a list uses mutually recursive functions
    contents and tolist. The definitions are fairly complicated. The ensures
    clauses are extensive because they help Chalice proves facts faster.
    Moreover, the tolist funciton cannot be simplified by defining it as its
    third ensures clause becauses this causes the termination check to fail.
   */

  /*@
    requires state();
    ensures  \result.length>0;
    ensures  \result == tolist(this);
    pure seq<int> contents()=tolist(left) + seq<int>{data} + tolist(right);

    ghost
    requires t->state();
    ensures  t!=null ==> \result == t.contents();
    ensures  t==null ==> \result == seq<int>{};
    ensures  \result == (t==null ? seq<int>{} : t.contents());
    pure seq<int> tolist(Tree t){
        if (t==null) {
            return seq<int>{};
        } else {
            unfold t.state();
            return t.tolist(t.left) + seq<int>{t.data} + t.tolist(t.right);
        }
    }  
  @*/

  /*@
    resource state_contains(seq<int> L)=state() ** contents()==L;
        
    resource contains(Tree t,seq<int>L)=t->state() ** L == tolist(t);

    pure boolean sorted_list(seq<int> s)=
      (\forall int i ; 1 < i && i < |s| ; s[i-1] <= s[i] );
      
    requires t->state();
    pure boolean sorted(Tree t)=t==null || sorted_list(t.contents());
  @*/

  //@ requires top!=null ** top.state();
  //@ ensures  contains(\result,tail(\old(top.contents())));
  //@ ensures  \old(sorted(top)) ==> sorted(\result);
  public Tree del_min(Tree top){
    //@ seq<int> orig_contents=top.contents();
    //@ seq<int> target_contents=tail(top.contents());
    //@ unfold top.state();
    if (top.left == null) {
      return top.right;
    } else {
      Tree cur, left;
      cur = top;
      left = top.left;
      //@ seq<int> cur_contents=orig_contents;
      //@ assert cur_contents == left.contents() + seq<int>{top.data} + tolist(top.right);
      //@ unfold left.state();
      /*@
      loop_invariant Perm(cur.left,100) ** Perm(cur.data,100) ** Perm(cur.right,100);
      loop_invariant cur.left==left ** cur.right->state() ;
      loop_invariant Perm(left.left,100) ** Perm(left.data,100) ** Perm(left.right,100);
      loop_invariant left.left->state() ** left.right->state();
      loop_invariant cur_contents == (tolist(left.left) + seq<int>{left.data} + tolist(left.right))
                                      + seq<int>{cur.data} + tolist(cur.right);
      loop_invariant wand:(cur.state_contains(tail(cur_contents)) -* top.state_contains(target_contents)); @*/
      while (left.left != null) /*@ with {
        create {} wand:(top.state_contains(target_contents) -* top.state_contains(target_contents));#\label{proof 1}#
      } @*/
      { /*@ Tree prev = cur;
            seq<int> prev_contents = cur_contents; */
        cur = left;
        left = cur.left;
        //@ assert left != null;
        /*@
        unfold left.state();
        assert left.left != null;
        cur_contents = tolist(left.left) + seq<int>{left.data} + tolist(left.right);
        cur_contents = cur_contents + seq<int>{cur.data} + tolist(cur.right);
        assert prev_contents.length > 0 ;
        assert cur_contents.length > 0 ;
        assert prev_contents == cur_contents + seq<int>{prev.data} + tolist(prev.right);
        create  {#\label{proof 2 begin}#
          use    prev_contents.length > 0 ;
          use    cur_contents.length > 0 ;
          use    Perm(prev.left,100)**Perm(prev.data,100);
          use    Perm(prev.right,100)**prev.right->state();
          use    prev.left==cur;
          use    prev_contents == cur_contents + seq<int>{prev.data} + tolist(prev.right);
          fold   prev.state();
          apply  wand:(prev.state_contains(tail(prev_contents)) -* top.state_contains(target_contents));
          qed    wand:(cur.state_contains(tail(cur_contents)) -* top.state_contains(target_contents));
        } #\label{proof 2 end}#
        @*/
        
        //@ assert left.left != null;
      }
      //@ assert false;
      cur.left = left.left;
      //@ fold cur.state();
      //@ assert cur.contents()==tail(cur_contents);
      //@ apply wand:(cur.state_contains(tail(cur_contents)) -* top.state_contains(target_contents));
      return top;
    }
  }
}

