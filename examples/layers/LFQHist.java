// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases LFQhistoryImplementation
//:: tools silicon
//:: options --check-history
/*
  verify with
    vct --silver=silicon --check-history LFQHist.java
 */

final class History {/*@
    ghost seq<int> q;

    modifies q;
    ensures  q==\old(q)+seq<int>{e};
    process  put(int e);

    modifies q;
    ensures  success ==> q==\old(q)+seq<int>{e};
    ensures  !success ==> q==\old(q);
    process  try_put(int e,boolean success);


    modifies q;
    ensures  \old(q)==seq<int>{e}+q;
    ensures  (\forall seq<int> q2;true;
                \old(q)+q2==seq<int>{e}+(q+q2));
             // the \forall works around a tool issue.
    process  get(int e);

@*/
}

final class Integer {

    int val;

    /*@
      ensures Value(val)**val==v;
    @*/
    Integer(int v) {
        val = v;
    }

}

final class Node {

    int val;

    AtomicNode next;

}


final class AtomicNode {

    Node ref;

    /*@
      ensures PointsTo(ref,1,n);
    @*/
    public AtomicNode(Node n) {
        ref = n;
    }

    void set(Node n) {
        this.ref = n;
    }

    Node get() {
        return this.ref;
    }

    boolean compareAndSet(Node e, Node v) {
        boolean res = (this.ref == e);
        if (res) {
            this.ref = v;
        }
        return res;
    }
}


final class Queue {

  /*@
    requires Value(hist) ** PointsTo(hist_active,1\2,true);
    ensures Value(hist)  ** PointsTo(hist_active,1\2,false)
      ** HPerm(hist.q,1);
  ghost void end_history(){
    atomic ( this ) {
      hist_active=false;
    }
  }
  @*/

    //@ ghost boolean hist_active;
    //@ ghost `History` hist;
    //@ ghost Node begin;
    AtomicNode head;
    AtomicNode tail;

    /*@ given `History` hist;
       requires HPerm(hist.q,1) ** hist.q==seq<int>{};
       ensures Value(this.hist) ** this.hist == hist
          ** PointsTo(hist_active,1\2,true); @*/
    public Queue() {
        //@ ghost this.hist=hist;
        //@ ghost hist_active=true;
        begin = new Node();
        begin.next = new AtomicNode(null);
        head = new AtomicNode(begin);
        tail = new AtomicNode(begin);
        last = begin;
        //@ fold reachable(begin,`head`.ref);
        //@ fold reachable(begin,`tail`.ref);
        //@ fold reachable(begin,last);
        //@ fold chain(begin,last,hist.q);
    }
    //@ ghost Node last;

  /*@
    // n is a final link in the history list.
    inline static resource final_link(Node n)=
      Value(n.next)**Value(n.next.ref)**n.next.ref!=null;

    resource reachable(Node n1,Node n2)=
      (n1!=n2 ==> final_link(n1) ** reachable(n1.next.ref,n2));
  @*/

  /*@

  // n is a link in the live data part of the history list.
  inline static resource chain_link(Node n,int val)=
    final_link(n) ** PointsTo(n.next.ref.val,1,val);

  // the final field ref is an atomic reference,
  // for which we have write permission.
  inline static resource RPerm(loc<AtomicNode> ref)=
    Value(ref) ** Perm(ref.ref,1);

  // the final field ref is an atomic reference,
  // for which we have write permission and know the value.
  inline static resource RPointsTo(loc<AtomicNode> ref, Node val)=
    Value(ref) ** Perm(ref.ref,1) ** ref.ref==val;

  resource chain(Node n1,Node n2,seq<int> vals)=
    n1 !=null ** n2 !=null ** (n1==n2 ? vals==seq<int>{} :
      (|vals|>0 ** chain_link(n1,head(vals))
       ** chain(n1.next.ref,n2,tail(vals))));

  resource csl_invariant() = Value(begin) **
    RPerm(`head`) ** ([read]reachable(begin,`head`.ref)) **
    RPerm(`tail`) ** ([read]reachable(begin,`tail`.ref)) **
    Perm(last,1) ** ([read]reachable(begin,last)) **
    //begin(context_everywhere)
    Perm(hist_active,1\2) ** Value(hist) ** (hist_active ==>
    HPerm(hist.q,1) ** chain(`head`.ref,last,hist.q))
    //end(context_everywhere)
    ** RPointsTo(last.next,null);
   */

    /*@
      given seq<int> ovals; given int nval;
      requires chain(n1,n2,ovals) ** chain_link(n2,nval)
        ** n2.next.ref==n3 ** n1 != n3 ** n2 != n3;
      ensures  chain(n1,n3,ovals+seq<int>{nval});
    @*/
    void append_lemma(Node n1, Node n2, Node n3) {
        int val = n2.next.ref.val;
        if (n1 == n2) {
            //@ unfold chain(n1,n2,ovals);
            //@ fold chain(n3,n3,seq<int>{});
            //@ fold chain(n2,n3,seq<int>{nval});
            return;
        } else {
            //@ unfold chain(n1,n2,ovals);
            //@ assert ovals == seq<int>{head(ovals)}+tail(ovals);
            append_lemma(n1.next.ref, n2, n3)
            /*@ with { ovals=tail(ovals); nval=nval; } @*/;
      /*@
        assert chain(n1.next.ref,n3,tail(ovals)+seq<int>{nval});
        assert seq<int>{head(ovals)}+(tail(ovals)+seq<int>{nval})
               == ovals+seq<int>{nval};
        fold chain(n1,n3,ovals+seq<int>{nval});
      @*/
            return;
        }
    }

    /*@
      given frac p;
      given process P;
      requires p!=none ** Value(hist) **  Hist(hist,p,P) **
        Value(`head`) ** Value(`tail`) ** PointsTo(hist_active,p\2,true);
      ensures  Value(`head`) ** Value(`tail`)
        ** Value(hist) ** PointsTo(hist_active,p\2,true)
        ** (\result!=null ==> Value(\result.val)
             ** Hist(hist,p,P * hist.get(\result.val)))
        ** (\result==null ==> Hist(hist,p,P));
    @*/
    Integer try_deq() {
        Node n1, n2;
        boolean tmp;
        Integer res = null;
        n1 = head.get();
        n2 = n1.next.get()/*@
    with {
      lemma_readable_or_last(this.begin,n1);
    }
    @*/;
        if (n2 != null) {
            tmp = head.compareAndSet(n1, n2)/*@
      with {
        if (`head`.ref==n1) {
          unfold chain(n1,last,hist.q);
//begin(actionblock)
 { action hist, p , P, hist.get(n2.val); hist.q=tail(hist.q); }
//end(actionblock)
        }
      }
      @*/;
            if (tmp) {
                res = new Integer(n2.val);
            }
        }
        return res;
    }

    /*@
      requires Perm(last,1\2) ** RPerm(last.next)
        ** ([read]reachable(n1,n2)) ** ([read]reachable(n1,last));
      ensures  Perm(last,1\2) ** RPerm(last.next)
        ** last==\old(last) ** last.next.ref==\old(last.next.ref)
        ** (n2!=last ==> final_link(n2) ** reachable(n1,n2.next.ref)
                                        ** Value(n2.next.ref.next));
    @*/
    void lemma_readable_or_last(Node n1, Node n2) {
        if (n2 != last) {
            //@ unfold [read]reachable(n1,n2);
            //@ unfold [read]reachable(n1,last);
            if (n1 == n2) {
                //@ unfold [read]reachable(n2,last);
                //@ unfold [read]reachable(n2.next.ref,last);
                //@ fold reachable(n2.next.ref,n2.next.ref);
                //@ fold reachable(n1,n2.next.ref);
            } else {
                //@ assert [read]reachable(n1.next.ref,last);
                lemma_readable_or_last(n1.next.ref, n2);
                //@ fold reachable(n1,n2.next.ref);
            }
        }
    }

    /*@
      given frac p;
      given process P;
      requires p!=none ** Value(`head`) ** Value(`tail`) ** Value(hist)
        ** Hist(hist,p,P) ** Perm(nn.val,1)
        ** RPointsTo(nn.next,null) ** PointsTo(hist_active,p\2,true);
      ensures  Value(`head`) ** Value(`tail`) ** Value(hist)
        ** PointsTo(hist_active,p\2,true) **
        (\result ? Hist(hist,p,P * hist.put(\old(nn.val))) :
           Hist(hist,p,P) ** Perm(nn.val,1) ** RPointsTo(nn.next,null));
    @*/
    boolean try_enq(Node nn) {
        Node n1, n2;
        boolean res = false;
        int val;
        n1 = tail.get();
        n2 = n1.next.get()/*@ with {
      lemma_readable_or_last(this.begin,n1);
    } @*/;
        if (n2 == null) {
            res = n1.next.compareAndSet(null, nn)/*@ with {
          lemma_readable_or_last(this.begin,n1);
        } then {
          if (\result) {
            val=nn.val;
            lemma_shift_last(n1,nn);
            { action hist, p, P, hist.put(\old(nn.val));
              hist.q=hist.q+seq<int>{\old(nn.val)}; }
          }
        } @*/;
        } else {
            tail.compareAndSet(n1, n2);
        }
        return res;
    }

    /*@
      requires Perm(last,1) ** last==n1 ** RPointsTo(last.next,n2);
      requires RPointsTo(n2.next,null) ** Perm(n2.val,1);
      requires Value(hist) ** HPerm(hist.q,1\2);
      requires Value(`head`) ** Perm(`head`.ref,1\2)
        ** chain(`head`.ref,last,hist.q) ** Value(begin)
        ** ([read]reachable(begin,last))
        ** ([read]reachable(begin,`head`.ref));
      ensures  Perm(last,1) ** last==n2 ** RPointsTo(last.next,null);
      ensures  Value(begin) ** ([read]reachable(begin,last));
      ensures  Value(hist) ** HPerm(hist.q,1\2);
      ensures  Value(`head`) ** Perm(`head`.ref,1\2)
        ** chain(`head`.ref,last,hist.q+seq<int>{\old(n2.val)})
        ** `head`.ref==\old(`head`.ref);
    @*/
    void lemma_shift_last(Node n1, Node n2) {
        last = n2;
        //@ assume `head`.ref != n2;
        //@ fold reachable(n2,n2);
        //@ fold reachable(n1,n2);
        lemma_reach_transitive(begin, n1, n2);
        append_lemma(head.ref, n1, n2)
        /*@ with { ovals=hist.q; nval=n2.val; } @*/;
    }

    /*@
      requires ([read]reachable(n1,n2)) ** ([read]reachable(n2,n3));
      ensures  reachable(n1,n3);
    @*/
    void lemma_reach_transitive(Node n1, Node n2, Node n3) {
        if (n1 != n2) {
            //@ unfold [read]reachable(n1,n2);
            lemma_reach_transitive(n1.next.ref, n2, n3);
            //@ assert reachable(n1.next.ref,n3);
            //@ fold reachable(n1,n3);
        } else {
            lemma_rebuild_full(n2, n3);
        }
    }

    /*@
      requires ([read]reachable(n1,n2));
      ensures  reachable(n1,n2);
    @*/
    void lemma_rebuild_full(Node n1, Node n2) {
        //@ unfold [read]reachable(n1,n2);
        if (n1 != n2) {
            lemma_rebuild_full(n1.next.ref, n2);
        }
        //@ fold reachable(n1,n2);
    }

}
