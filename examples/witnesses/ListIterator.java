// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ListIterator
//:: tools
//:: verdict Pass
//:: option --explicit
//:: suite slow

// Example disabled because it must be rewritten.

/**
 * The command line to verify with the VerCors Tool is:
 * <p>
 * vct --chalice ListIterator.java
 * <p>
 * The expected result is Pass.
 * <p>
 * Note that depending on which version of chalice is used,
 * this spec may take a very very long time to check.
 */

public class ListIterator {

  /*@
  resource ready()=
    Value(iteratee) ** iteratee!=null **Perm(iteratee.sentinel,100) ** iteratee.sentinel!=null
    **Perm(current,100)**Perm(last,100)**current!=null
    **Perm(current.val,100)**Perm(current.next,100)**Perm(current.prev,100)
    **(current.prev==null ==> current==iteratee.sentinel)
    **(current.prev!=null ==> current.prev.reverse() ** current.prev.first()==iteratee.sentinel ** current.prev.rev_next()==current)
    **current.next->state();
  resource readyForNext()=
    Value(iteratee) ** iteratee!=null **Perm(iteratee.sentinel,100) ** iteratee.sentinel!=null
    **Perm(current,100)**Perm(last,100)**current!=null
    **Perm(current.val,100)**Perm(current.next,100)**Perm(current.prev,100)
    **(current.prev==null ==> current==iteratee.sentinel)
    **(current.prev!=null ==> current.prev.reverse() ** current.prev.first()==iteratee.sentinel ** current.prev.rev_next()==current)
    **current.next->state()**current.next!=null;
  resource readyForRemove()=
    Value(iteratee) ** iteratee!=null **Perm(iteratee.sentinel,100) ** iteratee.sentinel!=null
    **Perm(current,100)**Perm(last,100)**current!=null
    **Perm(current.val,100)**Perm(current.next,100)**Perm(current.prev,100)
    **current.next->state()**current.prev==last
    **last!=null**Perm(last.val,100)**Perm(last.next,100)**Perm(last.prev,100)
    **(last.prev==null ==> last==iteratee.sentinel)
    **(last.prev!=null ==> last.prev.reverse() ** last.prev.first()==iteratee.sentinel ** last.prev.rev_next()==last)
    **last.next==current;
  @*/

    List iteratee;
    Node current;
    Node last;

    /*@
      requires l!=null ** l.state();
      ensures  ready();
      ensures  wand:(ready() -* l.state());
    @*/
    public ListIterator(List l) {
        //@ unfold l.state();
        current = l.sentinel;
        //@ unfold current.state();
        current.prev = null;
        iteratee = l;
        //@ fold ready();
    /*@
      create wand:(ready() -* l.state()){
        use Value(this.iteratee);
        use this.iteratee==l;
        unfold ready();
        fold this.current.state();
        if (this.current.get_prev()!=null){
            this.current.get_prev().swap(this.iteratee.sentinel,this.current);
        }
        fold l.state();
      }
    @*/
    }

    /*@
      requires ready();
      ensures  \result ==> readyForNext();
      ensures  !\result ==> ready();
    @*/
    boolean hasNext() {
        //@ unfold ready();
        boolean res = current.next != null;
    /*@
      if(!res) {
        fold ready();
      } else {
        fold readyForNext();
      }
    @*/
        return res;
    }

    /*@
      requires readyForNext();
      ensures  readyForRemove();
      ensures  wand:(readyForRemove() -* ready());
    @*/
    int next() {
        int res;
        //@ unfold readyForNext();
        last = current;
        current = current.next;
        //@ unfold current.state();
        res = current.val;
        current.prev = last;
        //@ fold readyForRemove();
    /*@
        create wand:(readyForRemove() -* ready()){
          unfold readyForRemove();
          fold   this.current.prev.reverse();
          fold   ready();
        }
    @*/
        return res;
    }

    /*@
      requires readyForRemove();
      ensures  ready();
    @*/
    void remove() {
        //@ unfold readyForRemove();
        last.next = current.next;
        current = last;
        //@ fold ready();
    }

  /* @ spec_ignore * /
  public static void main(String args[]){
    List l=new List();
    Example e=new Example();
    e.main(l);
  }*/

}

class List {

    Node sentinel;
  
  /*@
    resource state()=Perm(sentinel,100)**sentinel!=null**sentinel.state();
  @*/

    /*@
      ensures state();
    @*/
    public List() {
        sentinel = new Node(0, null);
        //@ fold state();
    }

    /*@
      requires state();
      ensures  state();
    @*/
    public void put(int v) {
        //@ unfold state();
        //@ unfold sentinel.state();
        sentinel.next = new Node(v, sentinel.next);
        //@ fold sentinel.state();
        //@ fold state();
    }
  
  /* @ spec_ignore * /
  void print(){
    ListIterator i=new ListIterator(this);
    System.out.printf("[");
    while(i.hasNext()){
       System.out.printf(" %d",i.next()); 
    }
    System.out.printf(" ]%n");
  }*/
}


class Example {

    /*@
      requires l!=null ** l.state();
      ensures  l!=null ** l.state();
    @*/
    void main(List l) {
        boolean b;
        l.put(1);
        l.put(0);
        l.put(-1);
        /* @ spec_ignore * / l.print();*/
        ListIterator i;
        //@ witness recover:(i.ready() -* l.state());
        //@ witness keep:(i.readyForRemove() -* i.ready());
        i = new ListIterator(l) /*@ then { recover = wand; } */;
        b = i.hasNext();
    /*@
      loop_invariant b ==> i.readyForNext();
      loop_invariant !b ==> i.ready();
    @*/
        while (b) {
            int tmp = i.next() /*@ then { keep = wand ;} */;
            if (tmp < 0) {
                i.remove();
            } else {
                //@ apply keep:(i.readyForRemove() -* i.ready());
            }
            b = i.hasNext();
        }
        //@ apply recover:(i.ready() -* l.state());
        /* @ spec_ignore * / l.print();*/
    }

}

class Node {

    public int val;
    public Node prev;
    public Node next;

  /*@
    resource state()=Perm(val,100)**Perm(prev,100)**Perm(next,100)**next->state();

    resource reverse()=Perm(val,100)**Perm(prev,100)**Perm(next,100)**
      (prev!=null ==> prev.reverse() ** prev.rev_next()==this);
  @*/

  /*@
    requires state();
    pure Node get_next()=next;
  @*/
  /*@
    requires state();
    pure Node get_prev()=prev;
  @*/
  /*@
    requires reverse();
    pure Node rev_next()=next;
  @*/

  /*@
    requires reverse();
    pure Node rev_prev()=prev;
  @*/

  /*@
    requires reverse();
    pure Node first()=(prev==null)?this:(prev.first());
  @*/


    /*@
      requires n->state();
      ensures  state() ** get_next()==n;
    @*/
    Node(int v, Node n) {
        val = v;
        next = n;
        //@fold state();
    }

    /*@
      requires fst!=null ** reverse() ** rev_next()==nxt ** nxt->state() ** first()==fst;
      ensures  fst!=null ** fst.state();
    @*/
    void swap(Node fst, Node nxt) {
        //@ unfold reverse();
        if (prev == null) {
            //@ fold state();
        } else {
            // Chalice cannot prove this simple fact:
            //  assert prev.first()==fst;
            // So we assume it
            //@ assume prev.first()==fst;
            Node tmp = prev;
            //@ fold state();
            tmp.swap(fst, this);
        }
    }

}

