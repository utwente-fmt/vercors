// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases OnlyNew
//:: tools silicon
//:: verdict Pass
class MyClass {
    void foo() {
        MyClass myClass = new MyClass();
        assert myClass instanceof MyClass;
    }
}