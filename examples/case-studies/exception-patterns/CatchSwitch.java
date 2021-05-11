//:: cases CatchSwitch
//:: tools silicon
//:: verdict Pass

class CatchSwitch {
    boolean randomBoolean();
    
    //@ ensures start <= \result && \result < end;
    int pickBetween(int start, int end);

    void m () {
        boolean throwB = randomBoolean();
        int picked = pickBetween(0, 3);
        int switchResult = 0;

        try {
            if (throwB) {
                throw new Exception();
            }
        } catch (Exception e) {
            switch (picked) {
                case 0:
                    switchResult = 10;
                    break;
                case 1:
                    switchResult = 11;
                    break;
                case 2:
                    switchResult = 12;
                    break;
                default:
                    //@ assert false;
                    break;
            }
        }

        if (throwB) {
            //@ assert picked == 0 ==> switchResult == 10;
            //@ assert picked == 1 ==> switchResult == 11;
            //@ assert picked == 2 ==> switchResult == 12;
        } else {
            //@ assert switchResult == 0;
        }
    }
}




