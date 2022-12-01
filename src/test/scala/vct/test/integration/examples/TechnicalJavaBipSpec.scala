package vct.test.integration.examples

import vct.test.integration.helper.JavaBipSpecHelper

class TechnicalJavaBipSpec extends JavaBipSpecHelper {
  failingTest(Seq("bipTransitionPreconditionFailed:false", "bipTransitionPreconditionUnsatisfiable"),
    "technical/javabip/transitionPrecondition/transitionPrecondition.json",
    "technical/javabip/transitionPrecondition/Main.java",
    "technical/javabip/transitionPrecondition/MyComponent.java",
  )

  failingTest("bipComponentInvariantNotEstablished:false",
    "technical/javabip/ComponentInvariantNotEstablished.json",
    "technical/javabip/ComponentInvariantNotEstablished.java"
  )
  failingTest("bipStateInvariantNotEstablished:false",
    "technical/javabip/StateInvariantNotEstablished.json",
    "technical/javabip/StateInvariantNotEstablished.java"
  )

  failingTest("bipComponentInvariantNotMaintained:false",
    "technical/javabip/ComponentInvariantNotMaintained.json",
    "technical/javabip/ComponentInvariantNotMaintained.java"
  )
  failingTest("bipStateInvariantNotMaintained:false",
    "technical/javabip/StateInvariantNotMaintained.json",
    "technical/javabip/StateInvariantNotMaintained.java"
  )
  failingTest("bipTransitionPostconditionFailure:false",
    "technical/javabip/TransitionPostconditionFailed.json",
    "technical/javabip/TransitionPostconditionFailed.java",
  )

  vercors should verify using silicon example "technical/javabip/BipGuardUnsatisfiablePrecondition.java"
  vercors should verify using silicon example "technical/javabip/BipGuardUsed.java"
  vercors should verify using silicon example "technical/javabip/BipGuardDataUsed.java"
}
