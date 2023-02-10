/*
This example fails because the method callMethodFive() does not have a decreases measure in its contract.
Thus, the final integer field cannot be computed, because its value is non-terminating, and our assertion fails.
The error message from this program does not in any way indicate where the real issue stems from, which is a point
for future improvement.
 */

public class finalIntegerNoDecreasesError {

    private final int FIVE_FROM_METHOD_CALL = callMethodFive();

    /* @pure */ private static int callMethodFive() {
        return 5;
    }

    private void test() {
        /*[/expect assertFailed:false]*/
        //@ assert FIVE_FROM_METHOD_CALL == 5;
        /*[/end]*/
    }

}