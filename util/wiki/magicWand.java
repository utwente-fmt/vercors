final class Tree {
  int value;
  int priority;
  Tree left;
  Tree right;

  /*@
  resource tree() = Perm(value, write) ** Perm(priority, write) ** Perm(left, write) ** Perm(right, write) 
    ** (left != null ==> left.tree()) ** (right != null ==> right.tree());

  requires t.tree();
  static pure boolean sorted(Tree t) 
  = t != null ==> \unfolding t.tree() \in 
    (t.left != null ==> sorted(t.left) && \unfolding t.left.tree() \in t.priority >= t.left.priority) 
    && (t.right!= null ==> sorted(t.right) && \unfolding t.right.tree() \in t.priority >= t.right.priority);
  @*/

  /*@
  requires t != null;
  requires t.tree();
  requires \unfolding t.tree() \in t.left != null;
  ensures \result != null ** \result.tree();
  ensures \result.tree() -* t.tree();
  @*/
  static Tree get_left(Tree t) {
    //@ unfold t.tree();
    Tree res = t.left;
    /*@
    create {
      use Perm(t.value, write) ** Perm(t.priority, write) ** Perm(t.left, write) ** Perm(t.right, write);
      use t.right != null ==> t.right.tree();
      use res == t.left;
      fold t.tree();
      qed res.tree() -* t.tree();
    }
    @*/
    return res;
  }

  /*@
  context t != null;
  context t.tree();
  requires \unfolding t.tree() \in t.left != null;
  @*/
  static boolean check_left_priority(Tree t, int max_priority) {
    Tree left = get_left(t);
    // now we have tree(left) and a wand tree(left) -* tree(t)
    //@ unfold left.tree();
    boolean res = left.priority <= max_priority;
    //@ fold left.tree();
    // apply tree(left) -* tree(t);
    //@ apply left.tree() -* t.tree();
    // now tree(left) is no longer directly available, but t.tree() is back
    return res;
  }

  /*@
  requires t != null ** t.tree();
  static boolean wand_helper(Tree t, int max_prio)
    = \unfolding t.tree() \in t.priority <= max_prio;
  @*/

  /*@
  yields int root_prio;
  requires t != null;
  requires t.tree() ** sorted(t);
  requires \unfolding t.tree() \in t.left != null;
  ensures \result != null ** \result.tree();
  ensures sorted(\result) && wand_helper(\result, root_prio);
  ensures \result.tree() ** sorted(\result) ** wand_helper(\result, root_prio) 
          -* 
          t.tree() ** sorted(t);
  @*/
  static Tree get_left_sorted(Tree t) {
    //@ unfold t.tree();
    //@ ghost root_prio = t.priority;
    Tree res = t.left;
    /*@
    create {
      // necessary parts for tree(t)
      use Perm(t.value, write) ** Perm(t.priority, write) ** Perm(t.left, write) ** Perm(t.right, write);
      use t.right != null ==> t.right.tree();
      use res == t.left;
      // necessary parts for sorted(t)
      use res != null;
      use t.priority == root_prio;
      use t.right != null ==> sorted(t.right) && \unfolding t.right.tree() \in t.priority >= t.right.priority;
      // assemble parts
      fold t.tree();
      qed res.tree() ** sorted(res) ** wand_helper(res, root_prio) 
          -* 
          t.tree() ** sorted(t);
    }
    @*/
    return res;
  }

  /*@
  context t != null;
  context t.tree() ** sorted(t);
  requires \unfolding t.tree() \in t.left != null;
  @*/
  static boolean check_left_priority_sorted(Tree t, int max_priority) {
    //@ ghost int root_prio;
    Tree left = get_left_sorted(t) /*@ then { root_prio = root_prio; } @*/;
    // now we have tree(left), sorted(left) and wand_helper(left, root_prio), as well as the wand
    //@ unfold left.tree();
    boolean res = left.priority <= max_priority;
    //@ fold left.tree();
    // apply (tree(left) ** sorted(left) ** wand_helper(left, root_prio)) -* (tree(t) ** sorted(t));
    //@ apply (left.tree() ** sorted(left) ** wand_helper(left, root_prio)) -* (t.tree() ** sorted(t));
    // now tree(left) is no longer directly available, but tree(t) is back and sorted
    return res;
  }
}
