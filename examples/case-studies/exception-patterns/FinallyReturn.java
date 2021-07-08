//:: cases FinallyReturn
//:: tools silicon
//:: verdict Pass

class FinallyReturn {
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







