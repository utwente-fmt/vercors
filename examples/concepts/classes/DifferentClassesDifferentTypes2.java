// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases DifferentClassesDifferentTypes2
//:: tools silicon
//:: verdict Fail

class A {}
class B {}

class DifferentClassesDifferentTypes2 {
    void foo() {
        A a = new A();
        B b = new B();
        /*[/expect assertFailed:false]*/
        assert a instanceof B;
        /*[/end]*/
    }
}
