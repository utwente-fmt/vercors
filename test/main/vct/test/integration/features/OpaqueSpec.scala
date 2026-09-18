package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class OpaqueSpec extends VercorsSpec {

  vercors should verify using anyBackend in "opaque examples" pvl """
opaque pure bool f(){
  return true;
}

void test1a(){
  [/expect assertFailed:false] assert f(); [/end]
}

void test1a(){
  assert reveal f();
}

opaque pure bool g() = true;

void test2a(){
  [/expect assertFailed:false] assert g(); [/end]
}

void test2a(){
  assert reveal g();
}
"""

  vercors should verify using anyBackend in "opaque example in C" c """
#include "stdbool.h"

/*@ opaque*/ /*@  pure*/ int f(){
  return 1;
}

void test1a(){
  //@ [/expect assertFailed:false] assert f() == 1; [/end]
}

void test1b(){
  //@ assert reveal f() == 1;
}

/*@
opaque pure bool g() = true;
*/

void test2a(){
  //@ [/expect assertFailed:false] assert g(); [/end]
}

void test2a(){
  //@ assert reveal g();
}
"""

}