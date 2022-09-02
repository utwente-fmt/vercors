package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalJavaBipSpec extends VercorsSpec {
  vercors should fail withCode "bipComponentInvariantNotMaintained" using silicon example "technical/javabip/ComponentInvariantNotMaintained.java"
  vercors should fail withCode "bipTransitionPostconditionFailure" using silicon example "technical/javabip/TransitionPostconditionFailed.java"
}
