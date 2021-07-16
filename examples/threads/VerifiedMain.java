// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases IncrThread
//:: suite skip-travis
//:: tool silicon
//:: verdict Pass
/* To verify this example:

  vct --silver=silicon SpecifiedThread.java Worker.java VerifiedMain.java
 */
public class Main {

    public static void main() {
        Worker w = new Worker(7);
        //@ assert w.input==7;
        w.start();
        w.join()/*@ with { p = 1; } @*/;
        //@ assert w.input==7;
        //@ open w.postJoin@Worker(1);
        //@ unfold w.postJoin@Worker(1);
        //@ assert w.output==8;
    }

}

