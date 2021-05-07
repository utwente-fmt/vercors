//:: cases ControlFlowFinallyReturn
//:: tools silicon
//:: verdict Pass

class ControlFlowFinallyReturn {
    //@ ensures \result == 3;
    int  m() {
        int x = 0;

        try {
            x += 1;   
        } finally {
            x += 2;
            return x;
        }

        //@ assert false;
    }
}







