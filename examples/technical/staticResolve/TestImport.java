package staticResolve;
public class TestImport {

    //@ requires Perm(Test.x, write);
    public void test() {

        int val = Test.x;

        int val = Test.five();

    }

}