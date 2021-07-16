//:: cases QualifiedNames
//:: tools silicon
//:: verdict Error

// In the future we want to support this syntax, so it is included in the test suite. When the syntax is implemented,
// this test will start passing, and then we can move it to the "fixed" folder.

class QualifiedNames {
    void foo() {
        Exception e = new java.lang.Exception();
    }

    void bar () {
        java.lang.Exception e = new Exception();
    }
}
