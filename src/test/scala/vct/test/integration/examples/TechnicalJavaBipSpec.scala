package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalJavaBipSpec extends VercorsSpec {
  vercors should fail withCode "bipStateInvariantNotMaintained" using silicon example "technical/javabip/StateInvariantNotMaintained.java"
  vercors should fail withCode "bipComponentInvariantNotMaintained" using silicon example "technical/javabip/ComponentInvariantNotMaintained.java"
  vercors should fail withCode "bipTransitionPostconditionFailure" using silicon example "technical/javabip/TransitionPostconditionFailed.java"
  vercors should fail withCode "bipStateInvariantNotEstablished" using silicon example "technical/javabip/StateInvariantNotEstablished.java"
  vercors should fail withCode "bipComponentInvariantNotEstablished" using silicon example "technical/javabip/ComponentInvariantNotEstablished.java"
}
