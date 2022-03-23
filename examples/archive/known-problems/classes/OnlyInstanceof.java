// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases OnlyInstanceof
//:: suite problem-fail
//:: tools silicon
//:: verdict Pass

class MyClass {
    void foo() {
        boolean b = this instanceof MyClass;
        assert b;
    }
}
