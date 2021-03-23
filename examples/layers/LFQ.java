// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases LFQatomicsImplementation
//:: tools silicon

/*
  verify with
    vct --silver=silicon LFQ.java
 */

final class Node {
  int val;
  AtomicNode next;
}


final class AtomicNode {
  Node val;
  
  // the set method is inlined as an atomic block
  void set(Node n){ this.val = n; }
  
  // the get method is inlined as an atomic block
	Node get(){ return this.val; }
	
  // the cas method is inlined as an atomic block
	boolean compareAndSet(Node e, Node v){
		boolean res=(this.val==e);
		if(res) {
			this.val=v;
		}
		return res;
	}

  /*@
    ensures PointsTo(val,1,n);
  @*/
  public AtomicNode(Node n){
    val=n;
  }
}

final class Integer {
  int val;
  /*@
    ensures PointsTo(val,1,v);
  @*/
  public Integer(int v){
    val=v;
  }
}

final class Queue {
/*@
  ensures Value(begin) ** Value(`head`) ** Value(`tail`);
@*/
  public Queue(){
    begin=new Node();
    begin.next=new AtomicNode(null);
    head=new AtomicNode(begin);
    tail=new AtomicNode(begin);
    last=begin;
    //@ fold reachable(begin,`head`.val);
    //@ fold reachable(begin,`tail`.val);
    //@ fold reachable(begin,last);
    //@ fold chain(begin,last);
  }


  //@ ghost Node begin;
  AtomicNode head;
  AtomicNode tail;
  //@ ghost Node last;

  /*@
    // n is a final link in the history list.
    inline static resource final_link(Node n)=
        Value(n.next)**Value(n.next.val);
    
    resource reachable(Node n1,Node n2)=
      (n1!=n2 ==> final_link(n1) ** reachable(n1.next.val,n2));
  @*/

  /*@
  
  // n is a link in the live data part of the history list.
  inline static resource chain_link(Node n)=
    final_link(n) ** Perm(n.next.val.val,1);
  
  // the final field ref is an atomic reference,
  // for which we have write permission.
  inline static resource RPerm(loc<AtomicNode> ref)=
    Value(ref) ** Perm(ref.val,1);
  
  // the final field ref is an atomic reference,
  // for which we have write permission and know the value.
  inline static resource RPointsTo(loc<AtomicNode> ref, Node val)=
    Value(ref) ** PointsTo(ref.val,1,val);
  
  resource chain(Node n1,Node n2)=n1 !=null ** n2 !=null **
    (n1!=n2 ==> chain_link(n1) ** chain(n1.next.val,n2));

  resource csl_invariant() = Value(begin) **
    RPerm(`head`) ** ([read]reachable(begin,`head`.val)) **
    RPerm(`tail`) ** ([read]reachable(begin,`tail`.val)) **
    Perm(last,1)   ** ([read]reachable(begin,last)) **
    chain(`head`.val,last) ** RPointsTo(last.next,null);
   */
   
  /*@
    requires chain(n1,n2) ** chain_link(n2) ** n2.next.val==n3
      ** RPointsTo(n3.next,null);
    ensures  chain(n1,n3) ** RPointsTo(n3.next,null);
  @*/
  void append_lemma(Node n1,Node n2,Node n3){
    if (n1==n2) {
      int v=n3.val;
      //@ fold chain(n3,n3);
      //@ fold chain(n2,n3);
    } else {
      //@ unfold chain(n1,n2);
      append_lemma(n1.next.val,n2,n3);
      //@ fold chain(n1,n3);
    }
  }
  
/*@ requires Value(`head`) ** Value(`tail`);
      ensures  Value(`head`) ** Value(`tail`)
      ** (\result != null ==> Perm(\result.val,1)); @*/
  Integer try_deq(){
    Node n1,n2; boolean tmp; Integer res=null;
    n1=head.get();
    n2=n1.next.get()/*@ with {
      lemma_readable_or_last(this.begin,n1); } @*/;
    if (n2!=null) {
      tmp=head.compareAndSet(n1,n2)/*@ with {
        if (`head`.val==n1) { unfold chain(`head`.val,last); } } @*/;
      if(tmp){ res=new Integer(n2.val); }
    }
    return res; }

  /*@
    requires Perm(last,1\2) ** RPerm(last.next) **
      ([read]reachable(n1,n2)) ** ([read]reachable(n1,last));
    ensures  Perm(last,1\2) ** RPerm(last.next) **
      last==\old(last) ** last.next.val==\old(last.next.val);
    ensures  n2!=last ==> final_link(n2) **
      reachable(n1,n2.next.val) ** Value(n2.next.val.next);
  @*/
  void lemma_readable_or_last(Node n1,Node n2){
    if (n2!=last){
      //@ unfold [read]reachable(n1,n2);
      //@ unfold [read]reachable(n1,last);      
      if (n1==n2) {
        //@ unfold [read]reachable(n2,last);
        //@ unfold [read]reachable(n2.next.val,last);
        //@ fold reachable(n2.next.val,n2.next.val);
        //@ fold reachable(n1,n2.next.val);
      } else {
        //@ assert [read]reachable(n1.next.val,last);
        lemma_readable_or_last(n1.next.val,n2);
        //@ fold reachable(n1,n2.next.val);
      }
    }
  }
  
/*@ requires Value(`tail`) ** Perm(nn.val,1)
             ** RPointsTo(nn.next,null);
    ensures  Value(`tail`) ** (!\result ==>
      Perm(nn.val,1) ** RPointsTo(nn.next,null)); @*/
  boolean try_enq(Node nn){
    Node n1,n2; boolean res=false;
    n1=tail.get();
    n2=n1.next.get()/*@ with {
      lemma_readable_or_last(this.begin,n1); } @*/;
    if (n2==null) {
      res=n1.next.compareAndSet(null,nn)/*@
        with { lemma_readable_or_last(this.begin,n1); }
        then { if (\result) { lemma_shift_last(n1,nn); } } @*/;
    } else { tail.compareAndSet(n1,n2); }
    return res; }

  /*@
    requires Perm(last,1) ** last==n1 ** RPointsTo(last.next,n2)
      ** RPointsTo(n2.next,null) ** Perm(n2.val,1)
      ** Value(`head`) ** Perm(`head`.val,1\2) ** chain(`head`.val,last)
      ** Value(begin) ** ([read]reachable(begin,last))
      ** ([read]reachable(begin,`head`.val));
    ensures  Perm(last,1) ** last==n2 ** RPointsTo(last.next,null)
      ** Value(begin) ** ([read]reachable(begin,last))
      ** Value(`head`) ** Perm(`head`.val,1\2)
      ** chain(`head`.val,last) ** `head`.val==\old(`head`.val);
  @*/
  void lemma_shift_last(Node n1,Node n2){
    last=n2;
    //@ fold reachable(n2,n2);
    //@ fold reachable(n1,n2);
    lemma_reach_transitive(begin,n1,n2);
    append_lemma(head.val,n1,n2);
  }
  
  /*@
    requires ([read]reachable(n1,n2)) ** ([read]reachable(n2,n3));
    ensures  reachable(n1,n3);
  @*/
  void lemma_reach_transitive(Node n1, Node n2, Node n3){
    if (n1!=n2){
      //@ unfold [read]reachable(n1,n2);
      lemma_reach_transitive(n1.next.val,n2,n3);
      //@ assert reachable(n1.next.val,n3);
      //@ fold reachable(n1,n3);
    } else {
      lemma_rebuild_full(n2,n3);
    }
  }
  
  /*@
    requires ([read]reachable(n1,n2));
    ensures  reachable(n1,n2);
  @*/
  void lemma_rebuild_full(Node n1, Node n2){
    //@ unfold [read]reachable(n1,n2);
    if (n1!=n2){
      lemma_rebuild_full(n1.next.val,n2);
    }
    //@ fold reachable(n1,n2);
  }
}

