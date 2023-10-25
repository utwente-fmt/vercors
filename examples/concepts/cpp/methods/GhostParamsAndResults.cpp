// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases GhostParamsAndResults
//:: tools silicon
//:: verdict Pass
	
/*@
	given int min;
	yields bool all_ge_min;
	requires min >= 0;
	ensures all_ge_min == (a >= min && b >= min);
*/
int add(int a, int b) {
	//@ ghost all_ge_min = a >= min && b >= min;
	return a + b;
}

int caller() {
	//@ ghost bool items_ge_5;
	int result = add(10, 12) /*@ given { min = 5 } */ /*@ yields { items_ge_5 = all_ge_min } */ ;
	//@ assert items_ge_5;
	return result;
}
