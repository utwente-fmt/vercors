// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases DifferentClassesDifferentTypes1
//:: tools silicon
//:: verdict Pass

class A {}
class B {}

class DifferentClassesDifferentTypes1 {
    void foo() {
        A a = new A();
        B b = new B();

        assert !(a instanceof B);
        assert !(b instanceof A);
        assert a instanceof A;
        assert b instanceof B;
    }
}
