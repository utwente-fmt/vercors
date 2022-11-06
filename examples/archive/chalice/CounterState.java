// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases CounterState

/**
  The command line to verify with the VerCors Tool is:
  
  vct --chalice --explicit CounterState.java
  
  The expected result is Pass.
*/
public class CounterState {

  public int val;
  
  /*@
  resource state(frac p, int v) = Perm(val, p) ** val == v;
  @*/
 
  /*@
  ensures state(write, v);
  @*/
  public CounterState(int v){
    val=v;
    //@ fold state(write, v);
  }

  /*@
  given int v;
  requires state(write, v);
  ensures state(write, v+1);
  @*/
  public void incr(){
    //@ unfold state(write, v);
    val = val + 1;
    //@ fold state(write, v+1);
  }

  /*@
    given int v;
    requires (state(write,v)) ** n>=0;
    ensures  state(write,v+n);
  @*/
  public void incr_loop(int n){
    //@ unfold state(write, v);
    int i=n;
    //@ loop_invariant Perm(val,write) ** v+n==val+i ** i>=0;
    while(i>0){
      val=val+1;
      i = i-1;
    }
    //@ fold state(write,v+n);
  }

  /*@
    given int v;
    requires (state(write,v)) ** n>=0;
    ensures  state(write,v+n);
  @*/
  public void incr_loop_fold(int n){
    int i=0;
    //@ loop_invariant state(write,v+i);
    //@ loop_invariant i<=n;
    while(i<n)
    {

      //@ unfold state(write,v+i);
      val = val + 1;
      i = i+1;
      //@ fold state(write,v+i);
    }
  }

  /*@
    given int v;
    requires (state(write,v)) ** n>=0;
    ensures  state(write,v+n);
  @*/
  public void incr_loop_call(int n){
    int i=0;
    //@ loop_invariant state(write,v+i);
    //@ loop_invariant i<=n;
    while(i<n)
    {
      incr() /*@ given { v=v+i } @*/;
      i = i+1;
    }
  }

  /*@
    ensures  \result!=null ** \result.state(write,v);
  @*/
  public CounterState create(int v){
    CounterState r=(new CounterState(v));
    return r;
  }

  /*@
    given frac p;
    given int v;
    requires state(p,v);
    ensures  state(p,v);
    ensures  \result!=null ** \result.state(write,v); 
  @*/
  public CounterState clone(){
    //@ unfold state(p,v);
    CounterState r=new CounterState(val);
    //@ fold state(p,v);
    return r;
  }

  /*@
    given frac p;
    given int v;
    requires state(p,v);
    ensures  \result==v;
  @*/
  public /*@ pure @*/ int get(){
    //@ unfold state(p,v);
    return val;
  }
  
  /*@
    given int v;
    requires (state(write,v)) ** n>=0;
    ensures  state(write,v+n);
  @*/
  public void incr_loop_get(int n){
    //@ unfold state(write,v);
    //@ ghost int i=0;
    CounterState c;
    c=create(0);
    //@ loop_invariant c!=null ** c.state(write,i);
    //@ loop_invariant c.get() given {p=write, v=i} == i;
    //@ loop_invariant c.get() given {p=write, v=i} <=n;
    // (...) other pure usages of get
    //@ loop_invariant Perm(val, write);
    //@ loop_invariant c.get() given {p=write, v=i} + v == val;
    while(c.get() /*@ given {p=write, v=i} @*/ < n)
    {
       val=val+1;
       c.incr() /*@ given { v = i } @*/;
       //@ ghost i = i + 1;
    }
    //@ fold state(write,v+n);
  }

}
