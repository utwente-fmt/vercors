// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Conditionals
//:: tools silicon
//:: verdict Pass

void onlyAssignInIf() {
	int num = 5;
	int result;
	if (num < 10) {
		result = 10;
	}
	//@ assert result == 10;
}

void matchIf() {
	int num = 5;
	int result = 0;
	if (num < 10) {
		result = 10;
	}
	//@ assert result == 10;
}

void matchElse() {
	int num = 5;
	int result = 0;
	if (num < 5) {
		result = 10;
	} else {
		result = 20;
	}
	//@ assert result == 20;
}