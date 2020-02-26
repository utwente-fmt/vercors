//class Exception {
//    int exception_id;
//    Exception(int _exception_id) {
//        exception_id = _exception_id;
//    }
//}

//import java.lang.Exception;

//final class MyException extends Exception { }

final class MyException { }

final class FooException { }

final class BarException { }

final class MyClass {
    int x;

    //@ requires Perm(x, 1);
    //@ ensures false; // Indicate that the function is not allowed to terminate by NOT throwing
    //@ signals (FooException e) Perm(x, 1) ** x == 4;
//    void foo() throws MyException, FooException {
    void foo() {
        int y = 3;
        x = y;
//        throw new MyException();
    }

    //@ requires Perm(x, 1);
    //@ ensures Perm(x, 1) ** x == 10;
    void bar() {
        int y = 3;
        x = y;
        try {
            foo();
            throw new MyException();
        } catch (MyException e) {
            x = 10;
            // TODO (Bob): Implement this
//        } catch (FooException | BarException | MyException e) {
//            x = 20;
        }
    }
}
