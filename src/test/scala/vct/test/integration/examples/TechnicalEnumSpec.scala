package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalEnumSpec extends VercorsSpec {
//  vercors should verify using silicon in "pvl/enums" pvl """
//    enum AB { A, B }
//  """

  vercors should verify using silicon in "pvl/enum return" pvl """
    enum AB { A, B }

    AB foo() {
      return AB.A;
    }
  """
}
