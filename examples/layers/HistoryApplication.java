// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases LFQhistoryApplication
//:: suite skip-travis
//:: tools silicon
//:: options --check-history

/* To verify this example:

  vct --silver=silicon --check-history HistoryApplication.java
  
  OLD: We skip this example during Travis builds because it is unstable:
  often is passes, soemtimes it fails.

  CURRENT: Can be parsed but not verified due to missing support for resource@object
 */

public class History {/*@
    ghost seq<int> q;
    
    modifies q;
    ensures  q==\old(q)+seq<int>{e};
    process  put(int e);
    
    modifies q;
    ensures  q==\old(q)+es;
    process  put_all(seq<int> es)=
      |es| == 0?empty:(put(head(es))*put_all(tail(es)));

    ensures put_all(es)*put(e)==put_all(es+seq<int>{e});
    ghost void put_lemma(seq<int> es,int e){
      if (|es|>0){
        put_lemma(tail(es),e);
        assert tail(es+seq<int>{e}) == tail(es)+seq<int>{e};
      }
    }

    modifies q;    ensures  \old(q)==seq<int>{e}+q;
    ensures  (\forall seq<int> q2;true;
                \old(q)+q2==seq<int>{e}+(q+q2));
             // the \forall works around a tool issue.
    process  get(int e);
    
    modifies q;    ensures  \old(q)==es+q;
    process  get_all(seq<int> es)=
      |es| == 0?empty:(get(head(es))*get_all(tail(es)));

    ensures get_all(es)*get(e)==get_all(es+seq<int>{e});
    ghost void get_lemma(seq<int> es,int e){ if (|es|>0){
      get_lemma(tail(es),e);
      assert |es|>0 ;
      assume tail(es+seq<int>{e}) == tail(es)+seq<int>{e};
    }}

    modifies q;    ensures  \old(q)+input==output+q;
    process  feed(seq<int> input,seq<int> output)=
      put_all(input)||get_all(output);

    modifies q;    ensures  \old(q)+input==output+q;
    process  feed2(seq<int> output,seq<int> input)=
      get_all(output)||put_all(input);

  @*/
}

public class Thread {

    //@ public resource joinToken(frac p);

    //@ public resource preFork(frac p)=true;

    //@ public resource postJoin(frac p)=true;

    public Thread() {
        //@ assume false;
    }

    /*@
      requires preFork(1);
      ensures  postJoin(1);
    @*/
    public void run();

    /*@
      requires preFork(1);
      ensures  joinToken(1);
    @*/
    public final void start();

    /*@
      given frac p;
      requires joinToken(p);
      ensures  postJoin(p);
    @*/
    public final void join();

}

class Sender extends Thread {

    AbstractQueue queue;
    int input[];
    //@ ghost seq<int> vals;
  
  /*@
    resource preFork(frac p)=p==write
      ** Value(this.queue) ** queue!=null ** Value(queue.hist)
      ** Value(this.input) ** PointsTo(queue.hist_active,1\4,true)
      ** Value(this.vals) ** input != null
      ** input.length==|vals|
      ** (\forall* int i; 0 <= i && i < |vals| ; Perm(input[i],1))
      ** (\forall  int i; 0 <= i && i < |vals| ; input[i]==vals[i])
      ** Hist(queue.hist,1\2,empty)
      ** true;
      
    resource postJoin(frac p)=p==write
      ** Value(this.queue) ** queue!=null ** Value(queue.hist)
      ** Value(this.input) ** PointsTo(queue.hist_active,1\4,true)
      ** Value(this.vals) ** input != null
      ** input.length==|vals|
      ** (\forall* int i; 0 <= i && i < |vals| ; Perm(input[i],1))
      ** (\forall  int i; 0 <= i && i < |vals| ; input[i]==vals[i])
      ** Hist(queue.hist,1\2,queue.hist.put_all(vals))
      ** true;
  @*/

    /*@
      given    seq<int>vals;
      requires input!=null ** input.length==|vals| ** queue!=null
        ** Value(queue.hist) ** Hist(queue.hist,1\2,empty)
        ** PointsTo(queue.hist_active,1\4,true)
        ** (\forall* int i; 0 <= i && i < |vals| ; Perm(input[i],1))
        ** (\forall  int i; 0 <= i && i < |vals| ; input[i]==vals[i]);
      ensures  Value(this.queue) ** queue!=null
        ** this.queue==queue
        ** Value(this.input) ** this.input==input
        ** Value(this.vals) ** this.vals==vals ** preFork(1);
    @*/
    public Sender(AbstractQueue queue, int[] input) {
        this.queue = queue;
        this.input = input;
        //@ ghost this.vals=vals;
        //@ fold this.preFork@Thread(1);
        //@ fold this.preFork@Sender(1);
    }

    public void run() {
        //@ unfold preFork@Sender(1);

        int N = input.length;
        int i = 0;

        //@ assert Hist(queue.hist,1\2,queue.hist.put_all(take(vals,i)));
    
    
    /*@
      loop_invariant 0 <= i ** i <= N 
      ** Value(this.queue) ** queue!=null ** Value(queue.hist)
      ** Value(this.input) ** PointsTo(queue.hist_active,1\4,true)
      ** Value(this.vals) ** input != null
      ** N==input.length ** N==|vals|
      ** (\forall* int k; 0 <= k < |vals| ; Perm(input[k],1))
      ** (\forall  int k; 0 <= k < |vals| ; input[k]==vals[k])
      ** Hist(queue.hist,1\2,queue.hist.put_all(take(vals,i)));
    @*/
        while (i < N) {
            queue.put(input[i]) /*@ with { p=1\2 ; P = queue.hist.put_all(take(vals,i));} @*/;
            //@ assert Hist(queue.hist,1\2,queue.hist.put_all(take(vals,i)+seq<int>{vals[i]}));
            //@ assert take(vals,i)+seq<int>{vals[i]}==take(vals,i+1);
            i = i + 1;
            //@ assert Hist(queue.hist,1\2,queue.hist.put_all(take(vals,i)));
        }
        //@ assert Hist(queue.hist,1\2,queue.hist.put_all(take(vals,N)));
        //@ assert take(vals,N)==vals;
        //@ fold this.postJoin@Thread(1);
        //@ fold this.postJoin@Sender(1);

    }


}


class Receiver extends Thread {

    AbstractQueue queue;
    int output[];
    //@ ghost seq<int> vals;
  
  /*@
    resource preFork(frac p)=p==write
      ** Value(this.queue) ** queue!=null ** Value(queue.hist)
      ** Value(this.output) ** PointsTo(queue.hist_active,1\4,true)
      ** Perm(this.vals,1) ** output != null
      ** (\forall* int i; 0 <= i && i < output.length ; Perm(output[i],1))
      ** Hist(queue.hist,1\2,empty)
      ** true;
      
    resource postJoin(frac p)=p==write
      ** Value(this.queue) ** queue!=null ** Value(queue.hist)
      ** Value(this.output) ** PointsTo(queue.hist_active,1\4,true)
      ** Perm(this.vals,1) ** output != null
      ** output.length==|vals|
      ** (\forall* int i; 0 <= i < |vals| ; Perm(output[i],1))
      ** (\forall  int i; 0 <= i < |vals| ; output[i]==vals[i])
      ** Hist(queue.hist,1\2,queue.hist.get_all(vals))
      ** true;
  @*/

    /*@
      requires queue!=null ** Value(queue.hist)
        ** Hist(queue.hist,1\2,empty) ** output != null
        ** PointsTo(queue.hist_active,1\4,true)
        ** (\forall* int i; 0 <= i < output.length ; Perm(output[i],1));
      ensures  Value(this.queue) ** queue!=null
        ** this.queue==queue ** Value(this.output)
        ** this.output==output ** preFork(1);
    @*/
    public Receiver(AbstractQueue queue, int[] output) {
        this.queue = queue;
        this.output = output;
        //@ fold this.preFork@Thread(1);
        //@ fold this.preFork@Receiver(1);
    }

    public void run() {
        //@ unfold preFork@Receiver(1); // skip(receiver)
        int N = output.length;
        int i = 0;
        //@ ghost vals=seq<int>{};
        /*@ loop_invariant Value(queue) ** Value(queue.hist)
         ** Value(output) ** output != null ** Perm(vals,1) ** 0 <= i <= N
         ** i==|vals| ** N==output.length
         ** PointsTo(queue.hist_active,1\4,true)
         ** (\forall* int k; 0 <= k < N ; Perm(output[k],1))
         ** (\forall int k; 0 <= k < i ; output[k]==vals[k])
         ** Hist(queue.hist,1\2,queue.hist.get_all(vals)); @*/
        while (i < N) {
            output[i] = queue.get()/*@ with {p=1\2 ; P = queue.hist.get_all(vals);} @*/;
            //@ ghost vals=vals+seq<int>{output[i]};
            i = i + 1;
        }
        //@ fold this.postJoin@Thread(1); // skip(receiver)
        //@ fold this.postJoin@Receiver(1); // skip(receiver)
    }
}

public class AbstractQueue {

//@ ghost History hist;
//@ ghost boolean hist_active;

    /*@ given History hist;
     requires HPerm(hist.q,1) ** hist.q==seq<int>{};
     ensures Value(this.hist) ** this.hist == hist
       ** PointsTo(hist_active,1\2,true); @*/
    public AbstractQueue() {
        //@ assume false;
    }


    /*@
      given frac p;
      given process P;
      requires Value(hist) ** p!=none
        ** PointsTo(hist_active,p/2,true) ** Hist(hist,p,P);
      ensures  Value(hist) ** p!=none
        ** PointsTo(hist_active,p/2,true) ** Hist(hist,p,P*hist.put(val));
    @*/
    public void put(int val);

    /*@ given frac p;   given process P;
          requires Value(hist) ** Hist(hist,p,P)
             ** p!=none ** PointsTo(hist_active,p/2,true);
          ensures  Value(hist) ** Hist(hist,p,P*hist.get(\result))
             ** p!=none ** PointsTo(hist_active,p/2,true);
    @*/
    public int get();

/*@
  requires Value(hist) ** PointsTo(hist_active,1\2,true);
  ensures Value(hist)  ** PointsTo(hist_active,1\2,false)** HPerm(hist.q,1);
  ghost void end_history();
@*/

}

public class Main {


    /*@
      given seq<int> contents;
      requires input!=null ** input.length==|contents|;
      requires output!=null ** output.length==|contents|;
      requires (\forall* int i; 0 <= i && i < |contents|;Perm(input[i],1));
      requires (\forall  int i; 0 <= i && i < |contents|;input[i]==contents[i]);
      requires (\forall* int i; 0 <= i && i < |contents|;Perm(output[i],1));
      requires (\forall  int i; 0 <= i && i < |contents|;output[i]==0);
    @*/
    public static void main(int[] input, int[] output) {
        History hist = new History();
        //@ ghost hist.q = seq<int>{};
        //@ create hist;
        AbstractQueue q = new AbstractQueue() /*@ with { hist=hist; } @*/;
        //@ split q.hist, 1\2, empty, 1\2, empty;


        Sender s = new Sender(q, input)/*@ with {vals=contents;} @*/;
        Receiver r = new Receiver(q, output);
        r.start();
        s.start();

        s.join()/*@ with { p = 1; } @*/;
        //@ open s.postJoin@Sender(1);
        //@ unfold s.postJoin@Sender(1);

        r.join()/*@ with { p = 1; } @*/;
        //@ open r.postJoin@Receiver(1);
        //@ unfold r.postJoin@Receiver(1);

        //@ assert Hist(hist,1\2,hist.put_all(s.vals));
        //@ assert Hist(hist,1\2,hist.get_all(r.vals));
        //@ merge hist, 1\2, hist.put_all(s.vals), 1\2, hist.get_all(r.vals);
        //@ ghost q.end_history();
        //@ destroy hist, hist.feed(s.vals,r.vals);

        // Derive some intermediate results.
        //@ assert seq<int>{} + s.vals == r.vals + hist.q;
        //@ assert hist.q == seq<int>{};
        // Assume a few extra rules about seqeunces.
        //@ assume (\forall seq<int> L; true ; seq<int>{} + L == L );
        //@ assume (\forall seq<int> L; true ; L + seq<int>{} == L );
        // Verify that the input and output are equal.
        //@ assert s.vals == r.vals;
    }
}

