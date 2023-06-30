// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Scoping
//:: tools silicon
//:: verdict Pass
void test() {
	int a = 10;
	int b = 10;

	{ // CompoundStatement { statements }
		a = 20;
		int b = 20;
		//@ assert a == 20;
		//@ assert b == 20;
	}
	//@ assert a == 20;
	//@ assert b == 10;
}