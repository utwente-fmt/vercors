// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Pointers
//:: tools silicon
//:: verdict Pass

// Examples taken from https://en.cppreference.com/w/cpp/language/pointer


// &x is not supported but I do need it for SYCL buffers
//@ context \pointer(ints, size, write);
void test(int ints[], int size, int* p) {
	//	char x = 'a';
	//	char* charPtr = &x;
	// assert *charPtr == x;

	int arr[5];
	//	int* arrPtr[5] = &arr;
	// assert arrPtr[4] == arr[4];

// Does not support arrays containing pointers?
//	int* ptrArr[5];
//	ptrArr[2] = p;


	void* voidPtr = nullptr;

	int n;
	//  int* p = &n;
	//  *p = 10;
  // assert n == 10;

  if (size >= 4) {
  	ints[3] = 5;
  }
   //@ assert size >= 4 ==> ints[3] == 5;
}