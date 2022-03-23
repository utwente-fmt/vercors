// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases FullThread Fibonacci

public class Thread {

  //@ resource joinToken(frac p);

  //@ resource preFork(frac p);

  //@ resource postJoin(frac p);
  
  
  public Thread(){
    //@ assume false;
  }
  
  /*@
    requires pre:preFork(write);
    ensures  post:postJoin(write);
  @*/
  public void run();

  /*@
    given frac p;
    requires pre:preFork(p);
    ensures  tok:joinToken(p);
  @*/
  public final void start();
  
  /*@
    given frac p;
    requires tok:joinToken(p);
    ensures  post:postJoin(p);
  @*/
  public final void joinWith();



}


