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