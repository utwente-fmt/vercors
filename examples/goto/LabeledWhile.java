class C {
    void m1() {
        int i = 0;
        //@ loop_invariant 0 <= i && i <= 5;
        l: while (i < 5) {
            i += 1;
        }
        assert i == 5;
    }

    void m2() {
        int i = 100;
        //@ loop_invariant 100 <= i && i <=110;
        l1: l2: while (i < 110) {
            i += 1;
        }
        assert i == 110;
    }

    void m3() {
        int i = 200;
        //@ loop_invariant 200 <= i;
        l1: l2:
        //@ loop_invariant i <= 230;
        while (i < 230) {
            i += 1;
        }
        assert i == 230;
    }

    void m4() {
        int i = -7;
        //@ loop_invariant -7 <= i;
        l1:
        //@ loop_invariant i != 0;
        l2:
        //@ loop_invariant i <= 7;
        while (i < 7) {
            i += 1;
            if (i == 0) {
                i += 1;
            }
        }
        assert i == 7;
    }
}