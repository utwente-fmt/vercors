// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Pointers
//:: tools silicon
//:: verdict Pass

//@ context \pointer(ints, size, write);
void test(int ints[], int size, int* p) {
	int* intsPtr = ints;
	//@ assert intsPtr == ints;

	void* voidPtr = nullptr;
}