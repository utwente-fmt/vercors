//:: cases ThrowFinallyThrowNewE
//:: tools silicon
//:: verdict Pass

class ThrowFinallyThrowNewE {
    int x;

    boolean randomBoolean();
    
    //@ context_everywhere Perm(x, write);
    //@ signals (Exception e) x == 5;
    //@ ensures false;
    int  m() throws Exception {
        boolean throwB = randomBoolean();

        try {
            x = 3;

            if (throwB) {
                throw new Exception();
            }
        } finally {
            x += 2;
            throw new Exception();
            //@ assert false;
        }

        //@ assert false;
    }
}








