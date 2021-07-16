// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases SimpleThread
public class SimpleThreadInstance extends SimpleThread {

    int input;

    //@ public resource joinToken()=true;

    //@ public resource preFork()=Value(input)**Perm(output,write);

    //@ public resource postJoin()=Value(input)**PointsTo(output,write,input+1);
    int output;
    /*@
      ensures preFork() ** Value(this.input) ** this.input==input;
    @*/
    public SimpleThreadInstance(int input) {
        this.input = input;
        //@ fold preFork@SimpleThread();
        //@ fold preFork@SimpleThreadInstance();
    }

    public void run() {
        //@ unfold preFork@SimpleThreadInstance();
        output = input + 1;
        //@ fold postJoin@SimpleThreadInstance();
    }

}

