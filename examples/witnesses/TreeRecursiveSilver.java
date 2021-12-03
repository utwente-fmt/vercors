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
    requires state();
    ensures this != null ==> \result.size > 0;
    pure seq<int> contents() = (this == null) ? seq<int>{} :
      \unfolding state() \in (left.contents() + seq<int>{data} + right.contents());
  @*/
  
  /*@
    requires state();
    ensures \result->state();
    ensures \result.contents() == \old(contents()).tail;
  @*/
  public Tree del_min() {
    //@ unfold state();
    if (left == null) {
      //@ assert left.contents().isEmpty;
      return right;
    } else {
      left = left.del_min();
      //@ fold state();
      return this;
    }
  }
}