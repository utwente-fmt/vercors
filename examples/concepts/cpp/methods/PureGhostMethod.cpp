// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases PureGhostMethod
//:: tools silicon
//:: verdict Pass

/*@
	ensures \result == a >= 0;
	pure bool isPositive(int a) = a >= 0;
*/

