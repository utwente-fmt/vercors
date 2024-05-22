// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Loops
//:: tools silicon
//:: verdict Pass

//@ requires size > 0;
//@ context \pointer(a, size, read);
//@ requires (\forall int i = 0 .. size; {: a[i] :} == 0);
//@ ensures \result == b;
int sumWithLast(int a[], int size, int b) {
	int sum = a[size-1] + b;
	//@ assert a[size-1] == 0;
	return sum;
}

//@ requires size > 2;
//@ context \pointer(a, size, write);
//@ ensures a[2] == 5;
void writeToArray(int a[], int size) {
	a[2] = 5;
}

//@ ensures \pointer(\result, 2, read);
//@ ensures \result[0] == 10 && \result[1] == 6;
int* createLocalArray() {
  int a[] = {4,5,6};
  //@ assert \pointer(a, 3, read) ** a[0] == 4 ** a[1] == 5 ** a[2] == 6;
  int b[2] = {7,a[2]};
  //@ assert \pointer(b, 2, write) ** b[0] == 7 ** b[1] == 6;
  int c[3];
  //@ assert \pointer(c, 3, write);
  c[1] = 10;
  b[0] = c[1];
  return b;
}