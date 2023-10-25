// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Permissions
//:: tools silicon
//:: verdict Pass


int amount = 0;

//@ requires Perm(amount, 1);
//@ ensures Perm(amount, 1) ** amount == x;
void set(int x) {
	amount = x;
}


