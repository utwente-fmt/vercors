// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Loops
//:: tools silicon
//:: verdict Pass


void whileLoop() {
	int numLoops = 10;
	//@ decreases numLoops;
	//@ loop_invariant numLoops >= 0;
	while(numLoops > 0) {
		numLoops--;
	}
}

//@ context_everywhere size >= 0;
//@ context_everywhere \pointer(arr, size, read);
void forArrayLoop(bool arr[], int size) {
	bool sum = true;

	//@ loop_invariant i >= 0 && i <= size;
	//@ loop_invariant sum == (\forall int j; j >= 0 && j < i; arr[j]);
	for(int i = 0; i < size; i++) {
		sum = sum && arr[i];
	}

  sum = true;
  //@ loop_invariant i >= 0 && i <= size;
  //@ loop_invariant sum == (\forall int j; j >= 0 && j < i; arr[j]);
	for(int i = 0; i < size;) {
    sum = sum && arr[i];
    i++;
  }

  for(int i = 0; ;) {
    i++;
    if (i >= size) break;
  }

  int i = 0;
  for(; ;) {
    i++;
    if (i >= size) break;
  }
}
