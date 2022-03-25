package vct.test.integration

import vct.test.integration.helper.VercorsSpec

class SilverDomainSpec extends VercorsSpec {
  vercors should verify using anyBackend example "concepts/domains/list.sil"
  vercors should fail withCode "preFailed" using anyBackend example "concepts/domains/option.sil"
}
