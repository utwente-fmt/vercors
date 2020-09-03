// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases Finally
//:: tools silicon
//:: verdict Pass

class Finally {
    void foo() {
        int x;
        try {
            x = 3;
        } finally {
            x = 4;
        }
        //@ assert x == 4;
    }
}