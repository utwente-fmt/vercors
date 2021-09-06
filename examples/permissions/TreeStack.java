// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TreeStack
//:: tools silicon
//:: suite medium
//:: verdict Pass

/*
    Note that this specification only validates access permissions.
    TODO:
      * extend state with all properties of a binary search tree.
     
*/

final public class Tree {

  public int data;
  public Tree left;
  public Tree right;

  /*@
    resource state()=Perm(data,1)**
	Perm(left,1)**Perm(right,1)**
	left->state()**right->state();
  @*/


  /*@
    requires t!=null ** t.state();
    ensures  \result->state();
  @*/
  public Tree del_min(Tree t){
        //@ unfold t.state();
	    Tree tt, pp, p;
	    p = t.left;
        Stack stk;
	   if (p == null) {
	       tt = t.right;
           return tt;
	   } else {
           //@ unfold p.state();
	       pp = t;
           tt = p.left;
           stk = null;
           //@ loop_invariant pp!=null;
           //@ loop_invariant p!=null;
           //@ loop_invariant Perm(p.left,1)**Perm(p.right,1);
           //@ loop_invariant Perm(p.data,1)**p.right->state();
           //@ loop_invariant Perm(pp.left,1)**Perm(pp.right,1);
           //@ loop_invariant Perm(pp.data,1)**pp.right->state();
           //@ loop_invariant p==pp.left;
           //@ loop_invariant tt==p.left;
           //@ loop_invariant tt->state();
           //@ loop_invariant stk->state();
           //@ loop_invariant stk==null ==> pp==t;
           //@ loop_invariant stk!=null ==> stk.get_root()==t;
           //@ loop_invariant stk!=null ==> stk.get_ref_left()==pp ;
	       while (tt != null) {
                stk=new Stack(pp,stk,t);
                //@ unfold tt.state();
		        pp = p;
                p = tt;
                tt = p.left;
	       }
	       tt = p.right;
           pp.left= tt;
           /*@ 
               fold pp.state();
			   
			   ghost
               loop_invariant pp!= null ** pp.state();
               loop_invariant stk->state();
               loop_invariant stk!=null ==> stk.get_ref_left() == pp ;
               loop_invariant stk==null ==> pp==t;
               loop_invariant stk!=null ==> stk.get_root()==t;
               while(stk!=null){
                 unfold stk.state();
                 pp = stk.ref;
                 stk = stk.tail;
                 fold pp.state();
               }
           */
           return t;
	   }
	 }
}

final class Stack {
  public Tree root;
  public Tree ref;
  public Stack tail;
  
  /*@
    resource state()=
        Perm(ref,1)**Perm(tail,1)**Perm(root,1)**tail->state()**ref!=null **
        Perm(ref.data,1)**Perm(ref.left,1)**Perm(ref.right,1)**ref.right->state()
        ** ((tail!=null)==>(ref==tail.get_ref_left() ** root==tail.get_root()))
        ** (tail==null ==> ref==root);
        
  @*/
  /*@
    requires ref!=null ** tail->state();
    requires Perm(ref.data,1)**Perm(ref.left,1)**Perm(ref.right,1)**ref.right->state();
    requires tail!=null ==> ref==tail.get_ref_left();
    requires tail!=null ==> root==tail.get_root();
    requires tail==null ==> ref==root;
    ensures  state() ** get_root()==root ** get_ref_left()==\old(ref.left);
  @*/
  public Stack(Tree ref,Stack tail,Tree root){
    this.ref=ref;
    this.tail=tail;
    this.root=root;
    //@ fold this.state();
  }

  /*@
    requires state();
    public pure Tree get_ref()=\unfolding state() \in ref; 
   @*/

  /*@
    requires state();
    public pure Tree get_ref_left()=\unfolding state() \in ref.left; 
   @*/

  /*@
    requires state();
    public pure Tree get_root()=\unfolding state() \in root;
   @*/
}


