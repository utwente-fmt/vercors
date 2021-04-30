// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases IncrThread IncrThread-E1 IncrThread-E2

public class Thread {

  //@ resource joinToken(frac p);

  //@ resource preFork(frac p);

  //@ resource postJoin(frac p);
  
  public Thread(){
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

