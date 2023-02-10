/*
This example fails to verify that the final integer's value is 5, because its value is set to a impure method. This
means that the verifier cannot know the concrete value of the integer, because the method is modular and we cannot
evaluate it.
 */

public class finalIntegerImpureError {

    private final int FIVE_FROM_METHOD_CALL = callMethodFive();

    //@ decreases;
    private static int callMethodFive() {
        return 5;
    }

    private void test() {
        /*[/expect assertFailed:false]*/
        //@ assert FIVE_FROM_METHOD_CALL == 5;
        /*[/end]*/
    }

}