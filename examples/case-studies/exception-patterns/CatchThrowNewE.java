//:: cases CatchThrowNewE
//:: tools silicon
//:: verdict Pass

class CatchThrowNewE {
    int x;

    boolean randomBoolean();

    //@ context_everywhere Perm(x, write);
    //@ signals (RuntimeException e) Perm(x, write) ** x == 5;
    //@ ensures x == 3;
    int m() {
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









