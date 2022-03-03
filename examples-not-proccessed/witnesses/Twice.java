// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Twice
//:: tools chalice
//:: options --explicit



/*
  command line
  
    vct --chalice --explicit Twice.java
    
  should produce
  
    The final verdict is Pass
 */
public class Twice {

  /*@
    resource state(frac p)=true;
  @*/
  
  /*@
    given frac p;
    requires in1:state(p);
    ensures out1:state(p);
  @*/
  void m();
  
  /*@
    given frac q;
    requires in2:state(q);
    ensures out2:state(q);
  @*/
  void twice(){
    m() /*@ label call1; with { p=q; in1=in2; } */;
    m() /*@ label call2; with { p=q; in1=call1.out1; }*/;
    //@ out2=call2.out1;
  }

}

