package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

case class Termination() extends VercorsSpec {
  vercors should verify using anyBackend in "simple terminating example" pvl """
    requires n >= 0;
    decreases n;
    pure int exp(int n) = n > 0 ? exp(n-1) + exp(n-1) : 1;
  """
}
