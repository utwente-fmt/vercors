package java.lang;

class Integer {
    //@ ensures intValue() == x;
    Integer(int x) {
        //@ inhale false;
    }

    /*@ ghost pure public int intValue(); @*/
}