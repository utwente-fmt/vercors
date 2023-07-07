// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases AbstractMethod
//:: tools silicon
//:: verdict Pass

/*@ ghost
	ensures \result == a >= 0;
	bool isPositive(int a);
*/

//@ requires b != 0;
//@ ensures \result == a / b;
int div(int a, int b);