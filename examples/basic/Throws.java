class Exception {
    int exception_id;
    Exception(int _exception_id) {
        exception_id = _exception_id;
    }
}

class MyClass {
    int x;

    //@ requires Perm(x, 1);
    //@ signals (Exception e) Perm(x, 1) ** x == 4 ** Perm(e.exception_id, 1) ** e.exception_id == 18;
    void foo() {
        int y = 3;
        x = y;
        throw new Exception(22);
    }
}