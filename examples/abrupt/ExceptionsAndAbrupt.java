// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ExceptionsAndAbrupt.java
//:: tools silicon
//:: verdict Pass

// This is an adaptation of a file from the examples directory of key.
// Original file path: KEY_ROOT/key/key.ui/examples/standard_key/java_dl/exceptions_java/MyClass2.java
// It has been adapted to use the vercors syntax for specifications.

public class C {
    //@ ensures \old(input_i) == 0 ==> \result == \old(input_i) + 2;
    //@ ensures \old(input_i) != 0 ==> \result == \old(input_i) + 3;
    public int foo(int input_i) {
        int i = input_i;
        l1: {
            try{
                if (i==0) {
                    break l1;
                }
                i = i + 1;
            } catch (Exception e) {
                // Nothing
                //@ assert false; // Is not executed
            } finally{
                i = i + 1;
            }
        }

        i = i + 1;

        return i;
    }

    //@ ensures \result == 2;
    public int bar() {
        IllegalArgumentException e = new IllegalArgumentException();
        int i = 0;
        try {
            throw e;
        } catch (IllegalStateException e0) {
            //@ assert false; // Should never happen
            return 0;
        } catch (RuntimeException e1) {
            i = 10;
            return 1;
        } finally {
            //@ assert i == 10; // Went through the RunTimeException first
            return 2;
        }
    }
}
