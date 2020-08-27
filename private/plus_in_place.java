class Test {
  void test(int r, int i, int a) {
    int res = r;
    //@ assume res == i*a;
    res += a;
    //@ assert res == i*a+a;
  }
}