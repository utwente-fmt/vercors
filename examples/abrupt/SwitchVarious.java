// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases SwitchVarious
//:: tools silicon
//:: verdict Pass

class C {
    void m1() {
        int x = -1;
        switch (1) {
            case 1:
                switch (3) {
                    case 3:
                        x = 33;
                        break;
                    case 4:
                        x = 44;
                        break;
                }
                break;
            case 2:
                x = 22;
                break;
            default:
                x = 100;
        }
        //@ assert x == 33;
    }

    void m2() {
        int x = -1;
        switch (1) {
            case 1:
                x = 11;
                break;
            case 2:
                x = 22;
                break;
        }
        //@ assert x == 11;
    }

    void m3() {
        int x = -1;
        switch (3) {
        }
        //@ assert x == -1;
    }

    void m4() {
        int x = -1;
        switch (3) {
            default:
                x = 33;
                break;
        }
        //@ assert x == 33;
    }

    void m5() {
        int x = -1;
        switch (3) {
            case 1:
                x = 11;
                break;
            case 2:
                x = 22;
                break;
            default:
                x = 33;
                break;
        }
        //@ assert x == 33;
    }

    //@ ensures 1 <= \result && \result <= 5;
    int pickBetween1And5();

    void m6() {
        int x = -1;
        switch (pickBetween1And5()) {
            case 0:
                x = 11;
                break;
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
                x = 22;
                break;
            case 6:
                x = 33;
                break;
        }
        //@ assert x == 22;
    }

    void m7() {
        int x = -1;
        switch (pickBetween1And5()) {
            case 1:
                x += 1;
            case 2:
                x += 1;
            case 3:
                x += 1;
            case 4:
                x += 1;
            case 5:
                x += 1;
                break;
        }
        //@ assert x > -1;
    }

    void m8() {
        int x = 0;
        switch (1) {
            case 0:
                x += 5;
            case 1:
                x += 20;
            case 2:
                x += 100;
        }
        //@ assert x == 120;
    }
}