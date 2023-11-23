package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalVeyMontExamplesSpec extends VercorsSpec {
  vercors should verify using silicon flag "--veymont-generate-permissions" example "technical/veymont/various.pvl"
}
