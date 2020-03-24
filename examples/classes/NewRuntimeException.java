// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases NewRuntimeException
//:: tools silicon
//:: verdict Pass

class MyClass {	
	void foo() {
		new RuntimeException();
	}
}
