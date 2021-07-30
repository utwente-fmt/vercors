// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases IncrThread-E2
//:: suite problem-fail
//:: tool silicon
//:: verdict Fail

/* To verify this example:

  vct --silver=silicon SpecifiedThread.java Worker.java VerifiedMain-E2.java
 */
public class Main {

  public static void main(){
    Worker w=new Worker(7);
    //@ assert w.input==7;
    w.start();
    w.join()/*@ with { p = 1; } @*/;
    //@ assert w.input==7;
    //@ open w.postJoin@Worker(1);
    // do not unfold w.postJoinWorker(1);
    //@ assert w.output==8;
  }

}

