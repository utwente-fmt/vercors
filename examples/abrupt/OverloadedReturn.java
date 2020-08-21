// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases OverloadedReturn
//:: tools silicon
//:: verdict Pass

class C {
    //@ ensures \result == x + 2;
    int m(int x) {
        try {
            return x + 2;
        } finally {

        }
    }

    //@ ensures \result == x + y + 10;
    int m(int x, int y) {
        try {
            return x + y + 10;
        } finally {

        }
    }
}