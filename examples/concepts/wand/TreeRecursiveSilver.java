// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TreeRecursiveSilver
//:: suite puptol
//:: tools silicon
/**
  
  The command line to verify with the VerCors Tool is:
  
  vct --silver=silicon TreeRecursiveSilver.java
  
  The expected result is Pass.
*/

/*@
requires t != null ==> t.state();
ensures t != null ==> \result.size > 0;
pure seq<int> contents(Tree t) =
  t == null ? [t:int] :
    \unfolding t.state() \in (contents(t.left) + [t.data] + contents(t.right));
@*/

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
    requires state();
    ensures \result != null ==> \result->state();
    ensures contents(\result) == \old(contents(this)).tail;
  @*/
  public Tree del_min() {
    //@ unfold state();
    if (left == null) {
      //@ assert contents(left).isEmpty;
      return right;
    } else {
      left = left.del_min();
      //@ fold state();
      return this;
    }
  }
}