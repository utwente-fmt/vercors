// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ListIterator
//:: tools
//:: verdict Pass
//:: option --explicit
//:: suite slow

// Example disabled because it must be rewritten.

/**

 The command line to verify with the VerCors Tool is:

 vct --chalice ListIterator.java

 The expected result is Pass.

 Note that depending on which version of chalice is used,
 this spec may take a very very long time to check.
 */

public class ListIterator {

  /*@
  resource ready() =
    Value(iteratee) ** iteratee!=null ** Perm(iteratee.sentinel, write) ** iteratee.sentinel!=null
    **Perm(current, write) ** Perm(last, write) ** current!=null
    **Perm(current.val, write) ** Perm(current.next, write) ** Perm(current.prev, write)
    **(current.prev == null ==> current == iteratee.sentinel)
    **(current.prev != null ==> current.prev.reverse() ** current.prev.first()==iteratee.sentinel
    ** current.prev.rev_next()==current) ** current.next?.state();

  resource readyForNext() =
    Value(iteratee) ** iteratee!=null ** Perm(iteratee.sentinel, write) ** iteratee.sentinel != null
    ** Perm(current, write) ** Perm(last, write) ** current != null
    ** Perm(current.val, write) ** Perm(current.next, write) ** Perm(current.prev, write)
    ** (current.prev == null ==> current == iteratee.sentinel)
    ** (current.prev != null ==> current.prev.reverse() ** current.prev.first() == iteratee.sentinel
    ** current.prev.rev_next() == current) ** current.next?.state() ** current.next != null;

  resource readyForRemove() =
    Value(iteratee) ** iteratee!=null **Perm(iteratee.sentinel, write) ** iteratee.sentinel != null
    ** Perm(current, write) ** Perm(last, write) ** current != null
    ** Perm(current.val, write) ** Perm(current.next, write) ** Perm(current.prev, write)
    ** current.next?.state() ** current.prev == last
    ** last != null ** Perm(last.val, write) ** Perm(last.next, write) ** Perm(last.prev, write)
    ** (last.prev == null ==> last == iteratee.sentinel)
    ** (last.prev != null ==> last.prev.reverse() ** last.prev.first() == iteratee.sentinel
    ** last.prev.rev_next() == last) ** last.next == current;
  @*/

    List iteratee;
    Node current;
    Node last;

    /*@
      requires l != null ** l.state();
      ensures ready();
      ensures ready() -* l.state();
    @*/
    public ListIterator(List l){
        //@ unfold l.state();
        current = l.sentinel;
        //@ unfold current.state();
        current.prev = null;
        iteratee = l;
        //@ fold ready();
    /*@ package ready() -* l.state() {
      unfold ready();
      fold this.current.state();
      if (this.current.get_prev() != null) {
          this.current.get_prev().swap(this.iteratee.sentinel, this.current);
      }
      fold l.state();
    } @*/
    }

    /*@
      requires ready();
      ensures \result ==> readyForNext();
      ensures !\result ==> ready();
    @*/
    boolean hasNext(){
        //@ unfold ready();
        boolean res=current.next!=null;
    /*@ ghost if (!res) {
        fold ready();
      }
      else {
        fold readyForNext();
      }
    @*/
        return res;
    }

    /*@
      requires readyForNext();
      ensures readyForRemove();
      ensures readyForRemove() -* ready();
    @*/
    int next(){
        int res;
        //@ unfold readyForNext();
        last = current;
        current = current.next;
        //@ unfold current.state();
        res = current.val;
        current.prev = last;
        //@ fold readyForRemove();
    /*@ package readyForRemove() -* ready() {
      unfold readyForRemove();
      fold   this.current.prev.reverse();
      fold   ready();
    } @*/
        return res;
    }

    /*@
      requires readyForRemove();
      ensures ready();
    @*/
    void remove(){
        //@ unfold readyForRemove();
        last.next=current.next;
        current=last;
        //@ fold ready();
    }

}

class List {

    Node sentinel;

  /*@
    resource state() =
      Perm(sentinel, write) ** sentinel!=null ** sentinel.state();
  @*/

    /*@
      ensures state();
    @*/
    public List(){
        sentinel=new Node(0,null);
        //@ fold state();
    }

    /*@
      requires state();
      ensures state();
    @*/
    public void put(int v){
        //@ unfold state();
        //@ unfold sentinel.state();
        sentinel.next=new Node(v,sentinel.next);
        //@ fold sentinel.state();
        //@ fold state();
    }
}


class Example {

    /*@
      requires l!=null ** l.state();
      ensures l!=null ** l.state();
    @*/
    void main(List l){
        boolean b;
        l.put(1);
        l.put(0);
        l.put(-1);
        ListIterator i;
        i=new ListIterator(l);
        b=i.hasNext();
    /*@
      loop_invariant b ==> i.readyForNext();
      loop_invariant !b ==> i.ready();
    @*/
        while(b){
            int tmp=i.next();
            if (tmp<0) {
                i.remove();
            } else {
                //@ apply i.readyForRemove() -* i.ready();
            }
            b=i.hasNext();
        }
        //@ apply i.ready() -* l.state();
    }

}

class Node {

    public int val;
    public Node prev;
    public Node next;

  /*@
    resource state() =
      Perm(val, write) ** Perm(prev, write) ** Perm(next, write) ** next?.state();

    resource reverse() =
      Perm(val, write) ** Perm(prev, write) ** Perm(next, write) **
      (prev != null ==> prev.reverse() ** prev.rev_next() == this);
  @*/

  /*@
    requires state();
    pure Node get_next() = \unfolding state() \in next;
  @*/
  /*@
    requires state();
    pure Node get_prev() = \unfolding state() \in prev;
  @*/
  /*@
    requires reverse();
    pure Node rev_next() = \unfolding reverse() \in next;
  @*/

  /*@
    requires reverse();
    pure Node rev_prev() = \unfolding reverse() \in prev;
  @*/

  /*@
    requires reverse();
    pure Node first() = \unfolding reverse() \in (prev==null ? this : prev.first());
  @*/


    /*@
      requires n?.state();
      ensures state() ** get_next()==n;
    @*/
    Node(int v, Node n){
        val=v;
        next=n;
        //@ fold state();
    }

    /*@
      requires fst!=null ** reverse() ** rev_next() == nxt ** nxt->state() ** first() == fst;
      ensures fst!=null ** fst.state();
    @*/
    void swap(Node fst,Node nxt){
        //@ unfold reverse();
        if (prev==null) {
            //@ fold state();
        } else {
            Node tmp=prev;
            //@ fold state();
            tmp.swap(fst,this);
        }
    }
}

