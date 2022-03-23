package integration.`new`

import integration.helper.VercorsSpec

class SilverDomainSpec extends VercorsSpec {
  vercors should verify using anyBackend example "domains/list.sil"
  vercors should fail withCode "preFailed" using anyBackend example "domains/option.sil"
}
