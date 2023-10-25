// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Namespaces
//:: tools silicon
//:: verdict Pass

int x;

namespace spaceA {

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

//@ context Perm(x, write);
int main() {
	x = 99;
  int varA = spaceA::incr();
  //@ assert varA == 100;
  //@ assert x == 90;
  int varB = spaceA::spaceB::incr();
  //@ assert varB == 92;
  spaceA::spaceB::doNothing();
  //@ assert x == 90;
  return 0;
}