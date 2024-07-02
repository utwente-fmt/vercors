package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class UniqueFieldsSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/unique/arrays.c"

  vercors should error withCode "wrongCType" in "multiple uniques" c """int main(/*@ unique<1> @*/ /*@ unique<2> @*/ int* x0){}"""
  vercors should error withCode "resolutionError:type" in "different uniques local" c """int main(/*@ unique<1> @*/ int* x0){ int* x1 = x0;}"""
  vercors should error withCode "resolutionError:type" in "multiple uniques parameter" c """int main(/*@ unique<1> @*/ int* x0, /*@ unique<2> @*/ int* x1){x1 = x0;}"""
  vercors should verify using silicon in "same uniques parameter" c """int main(/*@ unique<1> @*/ int* x0, /*@ unique<1> @*/ int* x1){x1 = x0;}"""
}
