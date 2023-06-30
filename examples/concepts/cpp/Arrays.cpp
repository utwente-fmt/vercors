// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Loops
//:: tools silicon
//:: verdict Pass

void test() {
	// Not supported atm
//	int arr[10];
	// assert \array(arr, 10);
	// assert \pointer(arr, 10, write);
//	arr[5] = 8;
	// assert arr[5] == 8;
}

//@ requires a != nullptr && size > 0;
//@ context \pointer(a, size, read);
//@ requires (\forall int i = 0 .. size; {: a[i] :} == 0);
//@ ensures \result == b;
int sumWithLast(int a[], int size, int b) {
	int sum = a[size-1] + b;
	//@ assert a[size-1] == 0;
	return sum;
}