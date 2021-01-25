//:: cases MethodSpec
//:: tools silicon
//:: pass m1
//:: fail m2
// verdict Pass CB.m1
// verdict Fail CB.m2


class CB {
    boolean p();

    void m1() {
        assert true;
    }

    void m2() {
        assert false; // Should be triggered
    }
}