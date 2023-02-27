public class finalIntegerViaMethodInvocation {

    private final int FIVE_FROM_METHOD_CALL = callMethodFive();

    //@ decreases;
    /* @pure */ private static int callMethodFive() {
        return 5;
    }

    private void test() {
        //@ assert FIVE_FROM_METHOD_CALL == 5;
    }

}