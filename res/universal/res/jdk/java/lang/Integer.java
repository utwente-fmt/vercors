package java.lang;

class Integer {
    final static int MIN_VALUE;
    final static int MAX_VALUE;

    //@ ensures intValue() == x;
    public Integer(int x) {
        //@ assume intValue() == x;
    }

    private Integer() {
    }

    /*@
    ghost
    pure public int intValue();

    ghost
    ensures \result != null;
    pure String nonNullString();
     */


    //@ ensures \result != null;
    public String toString() {
        String string;
        //@ ghost `string` = nonNullString();
        return string;
    }
}
