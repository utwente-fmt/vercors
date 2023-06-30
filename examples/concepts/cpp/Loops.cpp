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

// context_everywhere \pointer(arr, size + 1, read); -- does work for the foralll statement
//@ context_everywhere \pointer(arr, size, read);
void forArrayLoop(bool arr[], int size) {
	bool sum = true;
	//@ loop_invariant i >= 0 && i <= size;
	//@ loop_invariant sum == (\forall int j; j >= 0 && j < i; arr[i]);
	for(int i = 0; i < size; i++) {
		sum = sum && arr[i];
	}
}

void forLoop() {
	int numLoops = 10;
	int sum = 0;
	//@ loop_invariant i >= 0 && i <= numLoops;
	//@ decreases numLoops;
	//@ loop_invariant sum == i * (i+1) / 2;
	for(int i = 0; i < numLoops; i++) {
		numLoops--;
		sum += (i + 1);
	}
}
