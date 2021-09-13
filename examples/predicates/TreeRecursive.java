// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TreeRecursive
//:: tools silicon
//:: verdict Pass
/**
  
  The command line to verify with the VerCors Tool is:
  
  vct --silicon TreeRecursive.java
  
  The expected result is Pass.
*/

public class Tree {

  public int data;
  public Tree left;
  public Tree right;

  /*@
    final resource state()=Perm(data,1) **
    Perm(left,1) ** Perm(right,1) **
    left->state() ** right->state();
  @*/

  /*@
    requires t->state();
    ensures  t!=null ==> \result.length > 0;
    ghost public pure seq<int> contents(Tree t) {
      if(t==null){
          return seq<int>{};
      } else {
          unfold t.state();
          return contents(t.left)+seq<int>{t.data}+contents(t.right);
      }
    }
  */
  
  /*@
    requires t!=null ** t.state();
    ensures  \result->state();
    ensures  contents(\result)==tail(\old(contents(t)));
  @*/
  public Tree del_min(Tree t){
    //@ unfold t.state();
    if (t.left==null) {
      //@ assert contents(t.left) == seq<int>{};
      return t.right;
    } else {
      t.left=del_min(t.left);
      //@ fold t.state();
      return t;
    }
  }
}

