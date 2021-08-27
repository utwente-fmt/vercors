// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Getters
//:: tools
//:: options --explicit
/*
  This example shows how to use getters in combination with
  the witness encoding of predicates with arguments. When
  calling the tool on this file:
  
    vct --chalice --explicit Getters.java
    
  the result should be:
  
    The final verdict is Pass

*/

public class Getters {

    private int val;
    
    /*@
        resource state(frac p)=Perm(val,p);
    @*/
    
    /*@
        requires req:state(100);
        ensures  (ens:state(100)) ** get(req:ens,p:100)==v;
    @*/
    public void set(int v){
        //@ unfold req:state(100);
        val=v;
        //@ fold ens:state(100);
    }
    
    /*@
        given frac p;
        requires req:state(p);
    @*/
    public /*@ pure @*/ int get(){
        //@ unfold req:state(p);
        return val;
    }
    
    /*@
        ensures (ens:state(100)) ** get(req:ens,p:100)==v;
    @*/
    public Getters(int v){
        this.val=v;
        //@ fold ens:state(100);
    }

    /*@
        requires req:state(100);
        ensures  (ens:state(100)) ** get(req:ens,p:100)==\old(get(req:req,p:100))+1;
    @*/
    public void incr(){
        //@ unfold req:state(100);
        val=val+1;
        //@ fold ens:state(100);
    }
    
    /*@
        given    int p;
        given    int q;
        requires (req:state(p)) ** o != null ** (req2:o.state(q));
        ensures  \result==(get(req:req,p:p)==o.get(req:req2,p:q));
    */
    public /*@ pure @*/ boolean compare(Getters o){
        //@ unfold req:state(p);
        //@ unfold req2:o.state(q);
        return val==o.val;
    }
}


