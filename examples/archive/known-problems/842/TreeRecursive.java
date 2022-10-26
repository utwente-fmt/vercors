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
  resource state() =
    Perm(data,1) ** Perm(left,1) ** Perm(right,1) **
    left?.state() ** right?.state();
  @*/

  /*@
  ghost
  requires t->state();
  ensures t!=null ==> \result.size > 0;
  static public pure seq<int> contents(Tree t) {
    if(t==null) {
      return [t:int];
    } else {
      unfold t.state();
      return contents(t.left)+seq<int>{t.data}+contents(t.right);
    }
  }
  */
  
  /*@
  requires t!=null ** t.state();
  ensures \result->state();
  ensures contents(\result) == \old(contents(t)).tail;
  @*/
  public Tree del_min(Tree t){
    //@ unfold t.state();
    if (t.left==null) {
      //@ assert contents(t.left).isEmpty;
      return t.right;
    } else {
      t.left=del_min(t.left);
      //@ fold t.state();
      return t;
    }
  }
}

