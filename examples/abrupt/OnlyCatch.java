// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases OnlyCatch
//:: tools silicon
//:: verdict Pass

final class MyException extends Exception { }

final class FooException extends Exception { }

final class MyClass {
    int x;

    //@ signals (FooException e) true;
    // //@ ensures true;
    final int foo() {
        return 0;
    }

    //@ requires Perm(x, 1);
    //@ ensures Perm(x, 1) ** x == 40;
    final void bar() {
        try {
            x = 20;
            //@ assert x == 20;
            throw new MyException();
            assert false;
        } catch (MyException e) {
            //@ assert x == 20;
            x = 40;
            //@ assert x == 40;
        }
        //@ assert x == 40 ;
    }
}
