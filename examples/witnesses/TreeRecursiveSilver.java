// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TreeRecursiveSilver
//:: suite puptol
//:: tools silicon

/**
 * The command line to verify with the VerCors Tool is:
 * <p>
 * vct --silver=silicon TreeRecursiveSilver.java
 * <p>
 * The expected result is Pass.
 */

final class Tree {

    public int data;
    public Tree left;
    public Tree right;

  /*@
    public resource state()=Perm(data,write)**
	Perm(left,write)**Perm(right,write)**
	left->state()**right->state();
  @*/

  /*@
    requires t->state();
    ensures  t!=null ==> \result.length > 0;
    static seq<int> contents(Tree t)=(t==null)?seq<int>{}:
          \unfolding t.state() \in (contents(t.left)+seq<int>{t.data}+contents(t.right));
  */

    /*@
      requires t!=null ** t.state();
      ensures  \result->state();
      ensures  contents(\result)==tail(\old(contents(t)));
    @*/
    public Tree del_min(Tree t) {
        //@ unfold t.state();
        if (t.left == null) {
            //@ assert contents(t.left) == seq<int>{};
            return t.right;
        } else {
            t.left = del_min(t.left);
            //@ fold t.state();
            return t;
        }
    }
}

