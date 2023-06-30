// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ContextAndContextEverywhere
//:: tools silicon
//:: verdict Pass

//@ context b != 0;
// requires b != 0;
// ensures b != 0;
int div(int a, int b) {
	return a / b;
}

//@ context_everywhere number > 0;
// requires number > 0;
// ensures number > 0;
int factorial(int number) {
	int result = 1;
	// loop_invariant number > 0;
	for(int i = 2; i <= number; i++) {
		result *= i;
	}
	return result;
}