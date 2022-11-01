// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases SimpleThread
//:: suite problem-fail
//:: tools chalice silicon
//:: verdict Pass

/*
  vct --silver=silicon  SimpleThreadInstance.java SimpleThread.java SimpleThreadMain.java
  vct --chalice SimpleThreadInstance.java SimpleThread.java SimpleThreadMain.java
*/
public class Main {


  public void direct(){
    SimpleThreadInstance i=new SimpleThreadInstance(7);
    //@ assert i.input==7;
    i.run();
    //@ assert i.input==7;
    //@ open i.postJoin@SimpleThreadInstance();
    //@ unfold i.postJoin@SimpleThreadInstance();
    //@ assert i.output==8;
  }
  
  public void forkjoin(){
    SimpleThreadInstance i=new SimpleThreadInstance(7);
    //@ assert i.input==7;
    i.start();
    i.joinWith();
    //@ assert i.input==7;
    //@ open i.postJoin@SimpleThreadInstance();
    //@ unfold i.postJoin@SimpleThreadInstance();
    //@ assert i.output==8;
  }

}

