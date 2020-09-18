//:: cases ExceptionAbortsAssignment
//:: tools silicon
//:: verdict Pass

class C {
    //@ ensures false;
    //@ signals (RuntimeException e) true;
    int e();

    void m1() {
        int x = 0;
        try {
            x = e();
            //@ assert false; // Cannot be triggered, as exception is guaranteed
        } catch (RuntimeException e) {

        }
        //@ assert x == 0;
    }

//    void e2(int x);

//    void m2() {
//        int x = 3;
//        int y = (x = 4);
        //@ assert x == 4;
//        int x = 3;
//        try {
//            e2(x = e());
//        } catch (RuntimeException e) {
//
//        }
        //@ assert x == 3;
//    }
}
