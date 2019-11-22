class Java {
    boolean p();

    void foo() {
        int x = 0;
        boolean pp = p();
        //@ loop_invariant x == 0 || x == 4;
        while (pp) {
            try {
                try {
                    //@ assert x == 0
                    x = 1;
                    break;
                    x = 2;
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
