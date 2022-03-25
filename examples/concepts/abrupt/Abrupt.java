// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases Abrupt
//:: tools silicon
//:: verdict Pass

final class MyClass {
    boolean p();

    int foo() {
        int x = 0;

        my_if: if (p()) {
            x = 1;
            break my_if;
            x = 2;
            // The next one is never triggered:
            //@ assert false;
        }

        //@ assert x == 1 || x == 0;

        boolean pp = p();
        //@ loop_invariant x == 1 || x == 0 || x == 500;
        while (pp) {
            x = 5;
            pp = p();
            //@ loop_invariant x == 5 || x == 40;
            myLoop2: myLoop3: myLoop4: while(pp) {
                switch(3) {
                    default:
                        x = 4;
                        break myLoop3;
                    case 3:
                        x = 10;
                        break;
                }

                //@ assert x == 10;

                x = 40;

                //@ assert x == 40;
            }

            if (x == 41) {
                return 30;
            }

            //@ assert x == 40 || x == 5;

            x = 500;

            break;
        }

        //@ assert x == 500 || x == 1 || x == 0;

        return 31;
    }
}
