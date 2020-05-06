// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ExceptionsAndAbrupt.java
//:: tools silicon
//:: verdict Pass

// This is an adaptation of a file from the examples directory of key.
// Original file path: KEY_ROOT/key/key.ui/examples/standard_key/java_dl/exceptions_java/MyClass2.java
// It has been adapted to use the vercors syntax for specifications.

public class MyClass {
    //@ ensures i == 0 ==> \result == i + 3;
	//@ ensures i != 0 ==> \result == i + 2;
	public void blah(int i) {
		l1: {
			try{
				if (i==0) {
					break l1;
				}

				i = i + 1;
			} catch (Exception e) {
				// Nothing
			} finally{
				i = i + 1;
			}
		}

		i = i + 1;

		return i;
	}

	//@ ensures \result == 2;
	public int m() {
		IllegalArgumentException e = new IllegalArgumentException();
		try {
			throw e;
		} catch (IllegalStateException e0) {
			return 0;
		} catch (RuntimeException e1) {
			return 1;
		} finally {
			return 2;
		}
	}
}
