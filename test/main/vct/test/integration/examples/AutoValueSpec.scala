package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class AutoValueSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/autovalue/copy.pvl"
  vercors should verify using silicon example "concepts/autovalue/leak.pvl"
  vercors should error withCode "combinedAutoValue" example "concepts/autovalue/combination.pvl"
}
