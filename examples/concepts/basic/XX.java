class XX {

    /* TODO: The pure qualifier is ditched?
    /* pure int func() {
        return 3;
    }
     */

    /*@
    pure int func() = 3;
     */

    void yy() {
        zz();
        func();
    }

    void zz() {

    }
}