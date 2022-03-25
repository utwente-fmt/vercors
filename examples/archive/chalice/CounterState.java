// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases CounterState
//:: tools chalice
//:: options --explicit
/**
  The command line to verify with the VerCors Tool is:
  
  vct --chalice --explicit CounterState.java
  
  The expected result is Pass.
*/
public class CounterState {

  public int val;
  
  /*@
    resource state(frac p,int v)=Perm(val,p)**val==v;
  @*/
 
  /*@
    ensures res:state(100,v);
  @*/
  public CounterState(int v){
    val=v;
    //@ fold res:state(100,v);
  }

  /*@
    given int v;
    requires req:state(100,v);
    ensures  ens:state(100,v+1);
  @*/
  public void incr(){
    //@ unfold req:state(100,v);
    val = val + 1;
    //@ fold ens:state(100,v+1);
  }

  /*@
    given int v;
    requires (req:state(100,v)) ** n>=0;
    ensures  ens:state(100,v+n);
  @*/
  public void incr_loop(int n){
    //@ unfold req:state(100,v);
    int i=n;
    //@ loop_invariant Perm(val,100) ** v+n==val+i ** i>=0;
    while(i>0){
      val=val+1;
      i = i-1;
    }
    //@ fold ens:state(100,v+n);
  }

  /*@
    given int v;
    requires (req:state(100,v)) ** n>=0;
    ensures  ens:state(100,v+n);
  @*/
  public void incr_loop_fold(int n){
    int i=0;
    //@ loop_invariant inv:state(100,v+i);
    //@ loop_invariant i<=n;
    while(i<n)
    //@ with { inv = req ; } then { ens = inv; } 
    {
      //@ unfold inv:state(100,v+i);
      val = val + 1;
      i = i+1;
      //@ fold inv:state(100,v+i);
    }
  }

  /*@
    given int v;
    requires (req:state(100,v)) ** n>=0;
    ensures  ens:state(100,v+n);
  @*/
  public void incr_loop_call(int n){
    int i=0;
    //@ loop_invariant inv:state(100,v+i);
    //@ loop_invariant i<=n;
    while(i<n)
    //@ with { inv = req ; } then { ens = inv; } 
    {
      incr_call:incr/*@ v=v+i ; req=inv; */();
      /*@ inv=incr_call.ens; */
      i = i+1;
    }
  }

  /*@
    ensures  \result!=null ** ens:\result.state(100,v);
  @*/
  public CounterState create(int v){
    CounterState r=(new CounterState(v) /*@ with { ens = res ; } then { ens = res; } */);
    return r;
  }

  /*@
    given frac p;
    given int v;
    requires req:state(p,v);
    ensures  ens1:state(p,v);
    ensures  \result!=null ** ens2:\result.state(100,v); 
  @*/
  public CounterState clone(){
    //@ unfold req:state(p,v);
    CounterState r=new CounterState(val) /*@ then { ens2 = res ; } */;
    //@ fold ens1:state(p,v);
    return r;
  }

  /*@
    given frac p;
    given int v;
    requires req:state(p,v);
    ensures  \result==v;
  @*/
  public /*@ pure @*/ int get(){
    //@ unfold req:state(p,v);
    return val;
  }
  
  /*@
    given int v;
    requires (req:state(100,v)) ** n>=0;
    ensures  ens:state(100,v+n);
  @*/
  public void incr_loop_get(int n){
    //@ unfold req:state(100,v);
    //@ witness c_res:state(*,*);
    //@ int i=0;
    CounterState c;
    c=create(0)/*@ c_res = ens ; */;
    //@ loop_invariant c!=null ** inv:c.state(100,i);
    //@ loop_invariant c.get(p:100,v:i,req:inv)==i;
    //@ loop_invariant c.get(p:100,v:i,req:inv)<=n;
    //@ loop_invariant Perm(val,100);
    //@ loop_invariant c.get(p:100,v:i,req:inv)+v==val;
    while(c.get/*@ p=100; v=i; req=inv; */()<n)
    //@ with { inv = c_res ; }
    {
       val=val+1;
       c.incr/*@ v=i; req=inv; */() /*@ then { inv = ens; } @*/;
       //@ i = i + 1 ;
       // this should have worked isntead of the then above ? @ inv = incr_call.ens ;
    }
    //@ fold ens:state(100,v+n);
  }

}




