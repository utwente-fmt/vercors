package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalJavaBipSpec extends VercorsSpec {
  (vercors should fail
           withCode "bipStateInvariantNotMaintained:false"
           withReport "technical/javabip/StateInvariantNotMaintained.json"
           using silicon
           example "technical/javabip/StateInvariantNotMaintained.java")

  vercors should fail withCode "bipComponentInvariantNotMaintained:false" using silicon example "technical/javabip/ComponentInvariantNotMaintained.java"
  vercors should fail withCode "bipTransitionPostconditionFailure:false" using silicon example "technical/javabip/TransitionPostconditionFailed.java"
  vercors should fail withCode "bipStateInvariantNotEstablished:false" using silicon example "technical/javabip/StateInvariantNotEstablished.java"
  vercors should fail withCode "bipComponentInvariantNotEstablished:false" using silicon example "technical/javabip/ComponentInvariantNotEstablished.java"
  vercors should verify using silicon example "technical/javabip/BipGuardUnsatisfiablePrecondition.java"
  vercors should verify using silicon example "technical/javabip/BipGuardUsed.java"
  vercors should verify using silicon example "technical/javabip/BipGuardDataUsed.java"
}
