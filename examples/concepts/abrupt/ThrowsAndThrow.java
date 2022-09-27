// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases ThrowsAndThrow
//:: tools silicon
//:: verdict Pass

final class MyClass {
    MyClass() throws Exception {
        throw new Exception();
    }

    void foo() throws Exception, java.io.IOException {
        throw new Exception();
    }
}
