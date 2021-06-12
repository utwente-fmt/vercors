// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases OnlyInstanceof
//:: suite skip-travis
//:: tools silicon
//:: verdict Pass

class MyClass {
    void foo() {
        boolean b = this instanceof MyClass;
        assert b;
    }
}
