class Finally {
    void foo() {
        int x;
        try {
            x = 3;
        } finally {
            x = 4;
        }
        //@ assert x == 4;
    }
}