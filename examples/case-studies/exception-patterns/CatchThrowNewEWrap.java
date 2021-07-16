//:: cases CatchThrowNewEWrap
//:: tools silicon
//:: verdict Pass

class CatchThrowNewEWrap {
    int x;

    boolean randomBoolean();

    //@ context_everywhere Perm(x, write);
    //@ signals (Exception e) Perm(x, write) ** x == 5;
    //@ ensures x == 3;
    int m() throws Exception {
        boolean throwB = randomBoolean();

        try {
            x = 3;

            if (throwB) {
                throw new Exception();
            }
        } catch (Exception e) {
            x += 2;
            throw new RuntimeException(e);
            //@ assert false;
        }

        //@ assert x == 3;
    }
}








