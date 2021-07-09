//:: cases CatchThrowE
//:: tools silicon
//:: verdict Pass

class CatchThrowE {
    int x;
    Exception chosenE;

    boolean randomBoolean();
    
    //@ context_everywhere Perm(x, write);
    //@ context_everywhere Perm(chosenE, write);
    //@ signals (Exception e) Perm(x, write) ** Perm(chosenE, write) ** x == 5 ** e == chosenE;
    //@ ensures x == 3;
    void  m() throws Exception {
        boolean throwB = randomBoolean();

        try {
            x = 3;
            chosenE = new Exception();

            if (throwB) {
                throw chosenE;
            }
        } catch (Exception e) {
            x += 2;
            throw e;
            //@ assert false;
        }

        //@ assert x == 3;
    }
}








