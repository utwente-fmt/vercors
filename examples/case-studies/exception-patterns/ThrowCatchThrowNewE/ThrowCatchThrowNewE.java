//:: cases ThrowCatchThrowNewE
//:: tools silicon
//:: verdict Pass

class ThrowCatchThrowNewE {
    int x;

    boolean randomBoolean();
    
    //@ context_everywhere Perm(x, write);
    //@ signals (RuntimeException e) x == 5;
    //@ ensures x == 3;
    int  m() {
        try {
            x = 3;

            if (randomBoolean()) {
                throw new Exception();
            }
        } catch (Exception e) {
            x += 2;
            throw new RuntimeException();
            //@ assert false;
        }

        //@ assert x == 3;
    }
}









