// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases NamespacesDoNotFindWithoutNamespacePrefix
//:: tools silicon
//:: verdict Fail

namespace spaceA {

	namespace spaceB {
	  int b = 10;
	}

	int getB() {
	   return b;
	}
}