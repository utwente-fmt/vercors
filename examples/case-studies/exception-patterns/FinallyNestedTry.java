//:: cases FinallyNestedTry
//:: tools silicon
//:: verdict Pass


import java.io.IOException;

class FinallyNestedTry {
    int x;

    boolean randomBoolean();
    
    //@ signals (ArithmeticException.java e) Perm(x, write) ** (x == 31 || x == 32);
    //@ signals (ArrayStoreException e) Perm(x, write) ** (x == 111 || x == 112);
    //@ context_everywhere Perm(x, write);
    //@ ensures x == 32;
    int  m() {
        x = 0;

        boolean throwB1 = randomBoolean();
        boolean throwB2 = randomBoolean();

        try {
            if (throwB1) {
                x += 1;
                throw new ArithmeticException();
            }

            x += 2;
        } finally {
            try {
                x += 10;

                if (throwB2) {
                    throw new Exception();
                }
                
                x += 20;
            } catch (Exception e) {
                x += 100;
                throw new ArrayStoreException();
            }
        }
    }
}










