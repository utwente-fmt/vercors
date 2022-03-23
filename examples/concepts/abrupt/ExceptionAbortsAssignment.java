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
}
