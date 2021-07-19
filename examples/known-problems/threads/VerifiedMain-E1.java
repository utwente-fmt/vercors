// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases IncrThread-E1
//:: suite problem-fail
//:: tool silicon
//:: verdict Fail

/* To verify this example:

  vct --silver=silicon SpecifiedThread.java Worker.java VerifiedMain-E1.java
 */
public class Main {

  public void test(){
    main();
  }

  public static void main(){
    Worker w=new Worker(7);
    //@ assert w.input==7;
    w.start();
    w.join()/*@ with { p = 1; } @*/;
    //@ assert w.input==7;
    //@ open w.postJoin@Worker(1);
    //@ unfold w.postJoin@Worker(1);
    //@ assert w.output==7; // ERROR
  }

}

