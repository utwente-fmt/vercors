// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ContextAndContextEverywhere
//:: tools silicon
//:: verdict Pass

//@ context b != 0;
int div(int a, int b) {
  //@ assert b != 0;
	return a / b;
}

//@ context_everywhere number > 0;
int factorial(int number) {
	int result = 1;
	for(int i = 2; i <= number; i++) {
	  //@ assert number > 0;
		result *= i;
	}
	return result;
}