package staticResolve.subPackage;
import staticResolve.*;

public class DidNotForgetImport {

    //@ requires Perm(Test.x, write);
    public void test() {
        int val = Test.x;
        int val2 = Test.five();
    }

}