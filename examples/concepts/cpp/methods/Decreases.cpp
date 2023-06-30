// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Decreases
//:: tools silicon
//:: verdict Pass

//@ decreases number;
int factorial(int number) {
	if (number <= 1) {
		return 1;
	}
	return number * factorial(number - 1);
}