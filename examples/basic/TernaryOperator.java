class Test {
    void usage(boolean cond) {
        int result = cond ? expectEqual(cond, true) : expectEqual(cond, false);
    }

    /*@
    requires a == b;
    @*/
    int expectEqual(boolean a, boolean b) {
        return 0;
    }
}