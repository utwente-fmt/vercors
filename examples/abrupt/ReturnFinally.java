// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ReturnFinally
//:: tools silicon
//:: verdict Pass

// This is an adaptation of a file from the examples directory of key.
// Original file path: KEY_ROOT/key/key.ui/examples/standard_key/java_dl/exceptions_java/MyClass2.java
// It has been adapted and extended to use the vercors syntax for specifications.

public class ReturnFinally {
	int i;

	//@ requires Perm(i, write);
    //@ ensures Perm(i, write) ** \result == 0 ** i == 1;
    public int m() {
		i = 0;
		try {
			return i;
		} finally {
			i = 1;
		}
    }
}
