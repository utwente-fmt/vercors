// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Namespaces
//:: tools silicon
//:: verdict Pass

int x;

namespace spaceA {
	int x;

	//@ context Perm(x, write);
	//@ ensures x == 90;
	//@ ensures \result == \old(x) + 1;
	int incr() {
		int newX = x + 1;
		x = 90;
		return newX;
	}

	namespace spaceB {
		//@ context Perm(x, 0.5);
		//@ ensures \result == x + 2;
		int incr() {
  		return x + 2;
  	}

  	void doNothing() {};
	}
}

//@ context Perm(spaceA::x, write);
//@ context Perm(x, write);
int main() {
	x = 99;
	spaceA::x = 5;
	//@ assert spaceA::x == 5;
  int varA = spaceA::incr();
  //@ assert varA == 6;
  //@ assert spaceA::x == 90;
  int varB = spaceA::spaceB::incr();
  //@ assert varB == 92;
  spaceA::spaceB::doNothing();
  int varX = spaceA::x;
  //@ assert varX == 90;

  //@ assert x == 99;
  return 0;
}