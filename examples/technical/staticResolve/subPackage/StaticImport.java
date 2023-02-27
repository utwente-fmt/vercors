package staticResolve.subPackage;
import static staticResolve.Test.*;

public class StaticImport {

    //@ requires Perm(x, write);
    public void test() {
        int val = x;
        int val2 = five();
    }

}