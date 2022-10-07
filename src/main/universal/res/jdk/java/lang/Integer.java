package java.lang;

class Integer {
    //@ ensures intValue() == x;
    Integer(int x) {
        //@ inhale false;
    }

    /*@ pure @*/
    public int intValue() {
        //@ inhale false;
    }
}