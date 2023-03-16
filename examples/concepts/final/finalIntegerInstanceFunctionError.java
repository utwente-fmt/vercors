/*
This example fails because the CallMethodFive function is an instance function (e.g. it's not static). Thus, due to
encoding away constructors very early on, by the point at which we evaluate the final integer, the instance method
is a Local node rather than a ThisObject node, which leads to an error that doesn't allow us to "constantify" the
value of the final integer.
 */

public class finalIntegerInstanceFunctionError {

    private final int FIVE_FROM_METHOD_CALL = callMethodFive();

    //@ decreases;
    /* @pure */ private int callMethodFive() {
        return 5;
    }

    private void test() {
        /*[/expect assertFailed:false]*/
        //@ assert FIVE_FROM_METHOD_CALL == 5;
        /*[/end]*/
    }

}