// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases SimpleThread
public class SimpleThread {

  //@ resource joinToken();

  //@ resource preFork()=true;

  //@ resource postJoin()=true;
  
  /*@
    requires preFork();
    ensures  postJoin();
  @*/
  public void run();

  /*@
    requires preFork();
    ensures  joinToken();
  @*/
  public final void start();
  
  /*@
    requires joinToken();
    ensures  postJoin();
  @*/
  public final void joinWith();

}

