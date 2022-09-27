// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases WhileBreakFinally
//:: tools silicon
//:: verdict Pass

class C {
    void m() {
        while (true) {
            try {
                break;
            } finally {

            }
        }
    }
}
