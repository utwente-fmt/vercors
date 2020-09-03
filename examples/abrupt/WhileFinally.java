// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases WhileFinally
//:: tools silicon
//:: verdict Pass

class Java {
    boolean p();

    int foo() {
        int x = 0;
        boolean pp = p();
        //@ loop_invariant x == 0;
        while (pp) {
            try {
                try {
                    //@ assert x == 0;
                    x = 1;
                    break;
                } finally {
                    //@ assert x == 1;
                    x = 3;
                }
            } finally {
                //@ assert x == 3;
                x = 4;
            }
        }
        //@ assert (!(x == 0) && (x == 4)) || ((x == 0) && !(x == 4));
    }
}
