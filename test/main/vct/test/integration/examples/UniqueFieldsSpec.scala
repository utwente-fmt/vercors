package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class UniqueFieldsSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/unique/arrays.c"

  vercors should error withCode "???" in """int main(/*@ unique<1> @*/ /*@ unique<2> @*/ int* x0){}"""
}
