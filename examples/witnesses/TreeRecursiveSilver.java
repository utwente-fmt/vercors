// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TreeRecursiveSilver
//:: suite puptol
//:: tools silicon
/**
  
  The command line to verify with the VerCors Tool is:
  
  vct --silver=silicon TreeRecursiveSilver.java
  
  The expected result is Pass.
*/

final class Tree {

  public int data;
  public Tree left;
  public Tree right;

  /*@
  resource state() =
    Perm(data,write) **
    Perm(left,write) **
    Perm(right,write) **
    left->state() **
    right->state();
  @*/

  /*@
    requires t->state();
    ensures  t != null ==> \result.length > 0;
    pure seq<int> contents() = (this == null) ? seq<int>{} :
      \unfolding state() \in (left.contents() + seq<int>{data} + right.contents());
  @*/
  
  /*@
    requires t!=null ** t.state();
    ensures \result->state();
    ensures \result.contents() == \old(t.contents()).tail;
  @*/
  public Tree del_min(Tree t){
    //@ unfold t.state();
    if (t.left==null) {
      //@ assert t.left.contents() == seq<int>{};
      return t.right;
    } else {
      t.left = del_min(t.left);
      //@ fold t.state();
      return t;
    }
  }
}

