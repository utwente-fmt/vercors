// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases QualifiedNames
//:: tools silicon
//:: verdict Error
//:: suite problem-fail

class QualifiedNames {
    void foo() {
        Exception e = new java.lang.Exception();
    }

    void bar() {
        java.lang.Exception e = new Exception();
    }
}
