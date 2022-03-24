package vct.test.integration

import vct.test.integration.helper.VercorsSpec

class SummationSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/summation/TestCountFail.java"
  vercors should verify using silicon example "concepts/summation/TestCountPass.java"
  vercors should verify using silicon example "concepts/summation/TestFloat.java"
  vercors should verify using silicon example "concepts/summation/TestHist.java"
}
