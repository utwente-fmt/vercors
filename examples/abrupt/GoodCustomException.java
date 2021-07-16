// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases GoodCustomException
//:: tools silicon
//:: verdict Pass

final class GoodException extends Exception {
}

final class MyClass {
    void bar() throws GoodException {

    }
}