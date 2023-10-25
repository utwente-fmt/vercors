// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Predicates
//:: tools silicon
//:: verdict Pass

int amount = 0;

//@ resource pre_state() = Perm(amount, write);
//@ resource state(int value) = Perm(amount, write) ** amount == value;

//@ requires pre_state();
//@ ensures state(x);
void set(int x) {
	// pre_state();
	//@ unfold pre_state();
	// Perm(amount, write);
	amount = x;
	// Perm(amount, write) ** amount == x;
	//@ fold state(x);
	// state(x);
}

//@ requires pre_state();
//@ ensures state(50);
void manageAccount() {
	set(50);
}
