//class Exception {
//    int exception_id;
//    Exception(int _exception_id) {
//        exception_id = _exception_id;
//    }
//}

//import java.lang.Exception;

//final class MyException extends Exception { }

final class MyException { }

final class MyClass {
    int x;

    //@ requires Perm(x, 1);
    //@ ensures false; // Indicate that the function is not allowed to terminate by NOT throwing
    //@ signals (MyException e) Perm(x, 1) ** x == 4;
    //@ signals (FooException) Perm(x, 1) ** x == 4;
    void foo() {
        int y = 3;
        x = y;
        throw new MyException();
    }

    //@ requires Perm(x, 1);
    //@ ensures Perm(x, 1) ** x == 10;
    void bar() {
        int y = 3;
        x = y;
        try {
            throw new MyException();
        } catch (MyException e) {
            x = 10;
        } catch (FooException | BarException e) {
            x = 20;
        }
    }
}