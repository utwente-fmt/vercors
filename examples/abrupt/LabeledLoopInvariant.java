class C {
    void ok() {
        l: m: /*@ loop_invariant true; @*/ while (false) {}
    }

    void shouldBeOk() {
        //@ loop_invariant true;
        l: while (false) {}
    }
}