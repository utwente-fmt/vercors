//:: cases TryInCatch
//:: tools silicon
//:: verdict Pass

class TryInCatch {
    boolean randomBoolean();
    
    int  m() {
        int x = 0;
        boolean throwB1 = randomBoolean();
        boolean throwB2 = randomBoolean();

        try {
            if (throwB1) {
                x += 1;
                throw new Exception();
            }

            x += 2;
        } catch (Exception e1) {
            try {
                x += 10;

                if (throwB2) {
                    throw new Exception();
                }
                
                x += 20;
            } catch (Exception e2) {
                x += 100;
            }
        }

        if (throwB1) {
            if (throwB2) {
                //@ assert x == 111;
            } else {
                //@ assert x == 31;
            }
        } else {
            //@ assert x == 2;
        }
    }
}










