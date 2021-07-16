// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Fibonacci
//:: tools
// takes too much time to wait for...
//  :: tools chalice
//  :: options --explicit
//:: verdict Pass

// run with: vct --chalice --explicit FullThread.java Fibonacci.java

public class Fibonacci extends Thread {

    int input;

    //@ public resource preFork(frac p)=Value(input)**PointsTo(number,p,input);

    //@ public resource postJoin(frac p)=Value(input)**Perm(number,p);
    int number;

    /*@
      ensures pre:preFork(write) ** Value(this.input) ** this.input==input;
    @*/
    public Fibonacci(int input) {
        //@ witness sup:Fibonacci.preFork@Thread(*);
        this.input = input;
        number = input;
        //@ fold pre:this.preFork@Fibonacci(write);
    }

    public void run() {
        //@ unfold pre:preFork(write);
        if (!(input < 2)) {
            //output=fib(input);
            Fibonacci f1 = new Fibonacci(input - 1) /*@ label inst1 */;
            Fibonacci f2 = new Fibonacci(input - 2) /*@ label inst2 */;
            queue1:
            f1.start() /*@ with { p = write ; pre = inst1.pre;  } */;
            queue2:
            f2.start() /*@ with { p = write ; pre = inst2.pre;  } */;
            //@ assert f1.input==input-1;
            //@ assert f2.input==input-2;
            sync1:
            f1.joinWith() /*@ with { p = write ; tok = queue1.tok; } */;
            sync2:
            f2.joinWith() /*@ with { p = write ; tok = queue2.tok; } */;
            //@ witness res1:Fibonacci.postJoin(*);
            //@ witness res_inst1:Fibonacci.postJoin@Fibonacci(*);
            //@ res1=sync1.post ;
            //@ open res1:f1.postJoin@Fibonacci(write) { label dummy1 with { class_of = inst1.class_of ; } then { res_inst1 = member ; }};
            //@ unfold res_inst1:f1.postJoin@Fibonacci(write);
            //@ witness res2:Fibonacci.postJoin(*);
            //@ witness res_inst2:Fibonacci.postJoin@Fibonacci(*);
            //@ res2=sync2.post ;
            //@ open res2:f2.postJoin@Fibonacci(write) { label dummy2 with { class_of = inst2.class_of ; } then { res_inst2 = member ; }};
            //@ unfold res_inst2:f2.postJoin@Fibonacci(write);

            number = f1.number + f2.number;
        }
        //@ fold post:this.postJoin@Fibonacci(write);
    }

}


