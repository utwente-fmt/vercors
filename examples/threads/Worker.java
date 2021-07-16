// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases IncrThread IncrThread-E1 IncrThread-E2

public class Worker extends Thread {

    //@ public resource preFork(frac p)=Value(input)**p!=none**Perm(output,p);

    //@ public resource postJoin(frac p)=Value(input)**p!=none**PointsTo(output,p,input+1);

    int input;
    int output;
    /*@
      ensures preFork(1) ** Value(this.input) ** this.input==input;
    @*/
    public Worker(int input) {
        this.input = input;
        //@ fold this.preFork@Worker(1);
    }

    public void run() {
        //@ unfold preFork@Worker(1);
        output = input + 1;
        //@ fold this.postJoin@Worker(1);
    }

}

