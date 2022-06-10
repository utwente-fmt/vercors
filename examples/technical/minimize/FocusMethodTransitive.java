class FocusMethod {
    /*@ focus */
    void yy() {
        zz();
    }

    //@ ensures false;
    void zz() { }
}
