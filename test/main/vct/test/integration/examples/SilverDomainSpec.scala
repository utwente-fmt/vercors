package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class SilverDomainSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/domains/list.sil"
  vercors should verify using anyBackend example "concepts/domains/float.sil"
  vercors should failVerification withCode "preFailed:false" using anyBackend example "concepts/domains/option.sil"
}
