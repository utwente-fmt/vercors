class Java {
    boolean p();

    int foo() {
        boolean ppp = p();
        my_if: if (ppp) {
            break my_if;
        }

        int x = 0;
        boolean pp = p();
        //@ loop_invariant x == 0 || x == 4;
        while (pp) {
            try {
                try {
                    //@ assert x == 0;
                    x = 1;
                    break;
                    x = 2;
                    continue;
                    return 3;
                } finally {
                    //@ assert x == 1;
                    x = 3;
                }
            } finally {
                //@ assert x == 3;
                x = 4;
            }
        }
        //@ assert x == 0 || x == 4;
    }
}
