package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class InlinePatterns extends VercorsSpec {
  // https://github.com/utwente-fmt/vercors/issues/815

  vercors should verify using anyBackend in "inline pattern examples" pvl """
    pure int f(int i);
    pure int g(int i);

    void test1() {
      assume (∀int i; {:f(i):} == i);
      assert f(42) == 42;
    }

    void test2() {
      assume (∀int i; {:f(i):} == {:g(i):} && g(i) == 0);
      [/expect assertFailed:false] assert f(42) == 0; [/end]
    }

    void test3() {
      assume (∀int i; {:1:f(i):} == {:2:g(i):} && g(i) == 0);
      assert f(42) == 0;
    }

    void test4() {
      assume (∀int i; (∀int j; {:<:f(i):} + {:f(j):} == i + j));
      assert f(38) + f(39) == 77;
    }
  """

  vercors should verify using anyBackend in "inline pattern rewrite examples" pvl """
pure int f(int i);
pure int f2(int i, int j);
pure int g(int i);

void test1() {
  assume (∀int i; 0<=i && i<100; {:f(i+2):} == i+2);
  assert f(2) == 2;
  assert f(42) == 42;
  assert f(101) == 101;
  [/expect assertFailed:false] assert f(1) == 1; [/end]
}

void test1a() {
  assume (∀int i; 0<=i && i<100; {:f(i+2):} == i+2);
  [/expect assertFailed:false] assert f(-10) == -10; [/end]
}

void test1b() {
  assume (∀int i; 0<=i && i<100; {:f(i+2):} == i+2);
  [/expect assertFailed:false] assert f(102) == 102; [/end]
}

void test1c() {
  assume (∀int i; 0<=i && i<100; {:f(i+2):} == i+2);
  [/expect assertFailed:false] assert f(142) == 142; [/end]
}

void test1d() {
  assume (∀int i; 0<=i && i<100 && (i-8)%10 == 0; {:f(i+2):} == i+2);
  assert f(10) == 10;
  [/expect assertFailed:false] assert f(11) == 11; [/end]
}

void test2() {
  assume (∀int i, int j; 0<=i && i<100; {:f2(i+2, -5*j+2):} == i+2);
  assert f2(2, 2) == 2;
  assert f2(42, 7) == 42;
  assert f2(101, -3) == 101;
  [/expect assertFailed:false] assert f2(2, 3) == 2; [/end]
}

void test2a() {
  assume (∀int i, int j; 0<=i && i<100 && -1<=j && j<10; {:f2(i+2, -5*j+2):} == i+2);
  assert f2(101, 7) == 101;
  [/expect assertFailed:false] assert f2(101, 12) == 101; [/end]
}

void test2b() {
  assume (∀int i, int j; 0<=i && i<100 && -1<=j && j<10; {:f2(i+2, -5*j+2):} == i+2);
  assert f2(101, -5*9+2) == 101;
  [/expect assertFailed:false] assert f2(101, -5*10+2) == 101; [/end]
}

void test3() {
  assume (∀int i, int j; 0<=i && i<100 && 5<=j && j<8; {:f(i+2 + 100*(5*j+2)):} == i+2);
  assert (\let int i=2; (\let int j=5 ;f(i+2 + 100*(5*j+2)) == i+2));
  assert (\let int i=99; (\let int j=5 ;f(i+2 + 100*(5*j+2)) == i+2));
  assert (\let int i=0; (\let int j=5 ;f(i+2 + 100*(5*j+2)) == i+2));

  assert (\let int i=2; (\let int j=7 ;f(i+2 + 100*(5*j+2)) == i+2));
  assert (\let int i=99; (\let int j=7 ;f(i+2 + 100*(5*j+2)) == i+2));
  [/expect assertFailed:false] assert (\let int i=-1; (\let int j=7 ;f(i+2 + 100*(5*j+2)) == i+2)); [/end]
}

void test3a() {
  assume (∀int i, int j; 0<=i && i<100 && 5<=j && j<8; {:f(i+2 + 100*(5*j+2)):} == i+2);
  [/expect assertFailed:false] assert (\let int i=100; (\let int j=7 ;f(i+2 + 100*(5*j+2)) == i+2)); [/end]
}

void test3b() {
  assume (∀int i, int j; 0<=i && i<100 && 5<=j && j<8; {:f(i+2 + 100*(5*j+2)):} == i+2);
  [/expect assertFailed:false] assert (\let int i=1; (\let int j=4 ;f(i+2 + 100*(5*j+2)) == i+2)); [/end]
}

void test3c() {
  assume (∀int i, int j; 0<=i && i<100 && 5<=j && j<8; {:f(i+2 + 100*(5*j+2)):} == i+2);
  [/expect assertFailed:false] assert (\let int i=1; (\let int j=8 ;f(i+2 + 100*(5*j+2)) == i+2)); [/end]
}

void test3d() {
  assume (∀int i, int j; 0<=i && i<100 && 5<=j && j<8 && i+j==10; {:f(i+2 + 100*(5*j+2)):} == i+2);
  assert (\let int i=5; (\let int j=5 ;f(i+2 + 100*(5*j+2)) == i+2));
  [/expect assertFailed:false] assert (\let int i=6; (\let int j=5 ;f(i+2 + 100*(5*j+2)) == i+2)); [/end]
}

void test3e() {
  assume (∀int i, int j; 0<=i && i<100 && 5<=j && j<8 && i+j==10; {:f(-i+2 + -100*(5*j+2)):} == i+2);
  assert (\let int i=5; (\let int j=5 ;f(-i+2 + -100*(5*j+2)) == i+2));
  [/expect assertFailed:false] assert (\let int i=6; (\let int j=5 ;f(-i+2 + -100*(5*j+2)) == i+2)); [/end]
}

void test4(int[] a) {
  assume a != null && a.length == 100;
  inhale Perm(a[*], write);
  assume (∀int i; 0<=i && i<100; {:f(i):} == i);
  assume (∀int i, int j; 0<=i && i<10 && 2<=j && j<10; {:a[f(i+10*j)]:} == i);
  assert (\let int i=5; (\let int j=5 ; a[f(i+10*j)] == i));

  assume (∀int i, int j, int k; 0<=i && i<10 && 2<=j && j<10 && k%10==0; {:f2(a[i+10*j], k+5):} == i+k);
  assert (\let int i=5; (\let int j=5; (\let int k=20;  f2(a[i+10*j], k+5) == i+k)));
  [/expect assertFailed:false] assert (\let int i=5; (\let int j=1; (\let int k=20;  f2(a[i+10*j], k+5) == i+k))); [/end]
}

void test5(seq<int> a) {
  assume |a| == 100;
  assume (∀int i; 0<=i && i<100; {:f(i):} == i);
  assume (∀int i, int j; 0<=i && i<10 && 2<=j && j<10; {:a[f(i+10*j)]:} == i);
  assert (\let int i=5; (\let int j=5 ; a[f(i+10*j)] == i));

  assume (∀int i, int j, int k; 0<=i && i<10 && 2<=j && j<10 && k%10==0; {:f2(a[i+10*j], k+5):} == i+k);
  assert (\let int i=5; (\let int j=5; (\let int k=20;  f2(a[i+10*j], k+5) == i+k)));
  [/expect assertFailed:false] assert (\let int i=5; (\let int j=1; (\let int k=20;  f2(a[i+10*j], k+5) == i+k))); [/end]
}
  """

}
