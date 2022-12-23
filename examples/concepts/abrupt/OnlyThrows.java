// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases OnlyThrows
//:: tools silicon
//:: verdict Pass

final class MyClass {
    MyClass() throws Exception, java.io.IOException {}

    void foo() throws Exception, java.io.IOException { }
}
