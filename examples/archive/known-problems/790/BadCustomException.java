// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases BadCustomException
//:: tools silicon
//:: verdict Error

final class BadException {}

final class MyClass {
    void bar() throws BadException {

    }
}
