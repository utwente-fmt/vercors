package staticResolve;

public class Test {

    static int x = 5;

    //@ requires Perm(x, write);
    public static int five() {
        return x;
    }

    public Test () {
        String test = null;
    }
}