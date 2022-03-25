// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases CastExample
//:: tools silicon
//:: verdict Pass

/**
  This example shows how a binary tree with internal <code>Node</code>s
  and <code>Leaf</code>s can be specified if access is by means of casting
  rather than through a visitor pattern.
*/
class Node {
  int key;
  Object left;
  Object right;
  
  //@ requires state(left) ** state(right);
  //@ ensures state(this);
  public Node(int key,Object left,Object right){
    this.key=key;
    this.left=left;
    this.right=right;
    //@ assert this instanceof Node;
    //@ assert !(this instanceof Leaf);
    //@ fold state_rec(this);
  }
  
}

class Leaf {
  int other;
}

/*@
resource state_rec(Object t) = t != null **
  (t instanceof Leaf || t instanceof Node) **
  (t instanceof Leaf ==> Perm(((Leaf)t).other,1)) **
  (t instanceof Node ==>
     Perm(((Node)t).key,1) **
     Perm(((Node)t).left,1) ** state(((Node)t).left) **
     Perm(((Node)t).right,1) ** state(((Node)t).right));

inline resource state(Object t) = t != null ** state_rec(t);
@*/

class Test {
  //@ requires state(tt);
  int find(Object tt,int key){
    Object t=tt;
    //@ loop_invariant state(t);
    while(t instanceof Node){
      Node n=(Node)t;
      //@ unfold state_rec(n);
      if (key < n.key) {
        t=n.left;
      } else {
        t=n.right;
      }
    }
    //@ unfold state_rec(t);
    Leaf l=(Leaf)t;
    return l.other;
  }
}