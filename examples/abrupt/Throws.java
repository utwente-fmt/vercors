// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases Throws
//:: tools silicon
//:: verdict Pass

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

final class MyClass {
    int x;

    //@ requires Perm(x, 1);
    //@ signals (FooException e) Perm(x, 1) ** x == 10;
    //@ ensures Perm(x, 1) ** x == (\old(x) + 1);
    final void foo() {
        x = x + 1;
    }

    //@ requires Perm(x, 1);
    //@ ensures Perm(x, 1) ** (x == 40 || x == 50);
    final void bar() {
        try {
            foo();
            x = 20;
            throw new MyException();
            //@ assert false;
            x = 30;
        } catch (MyException e) {
            //@ assert x == 20;
            x = 40;
            //@ assert x == 40;
        } catch (FooException f) {
            //@ assert x == 10;
            x = 50;
            //@ assert x == 50;
        }
        //@ assert x == 40 || x == 50;
    }
}
