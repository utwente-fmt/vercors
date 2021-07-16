// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases simpleExamples
//:: tools silicon
//:: verdict Pass

// vct --silicon SimpleExamples.java 
// escjava2 SimpleExamples.java

public class SimpleExamples {

    /*@ ensures \result == x + 1 ; @*/
    public static int incr(int x) {
        int y = x + 1;
        return y;
    }

}

