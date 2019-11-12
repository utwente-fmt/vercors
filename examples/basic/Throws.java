class Exception {}

class MyClass {
    int x;

    //@ requires Perm(x, 1);
    //@ signals (Exception e) Perm(x, 1) ** x == 4;
    void foo() {
        x = 4;
        throw new Exception();
    }
}