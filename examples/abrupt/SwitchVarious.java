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
}