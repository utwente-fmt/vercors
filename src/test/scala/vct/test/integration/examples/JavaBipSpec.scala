package vct.test.integration.examples

import vct.test.integration.helper.JavaBipSpecHelper

class JavaBipSpec extends JavaBipSpecHelper {
  failingTest(Seq(
      "bipComponentInvariantNotMaintained:false",
      "bipComponentInvariantNotMaintained:false",
      "bipStateInvariantNotMaintained:false",
      "bipStateInvariantNotMaintained:false",
      "bipComponentInvariantNotMaintained:false"
    ),
    "concepts/javabip/casinoBroken/casinoBroken.json",
    "concepts/javabip/casinoBroken/Main.java",
    "concepts/javabip/casinoBroken/Constants.java",
    "concepts/javabip/casinoBroken/Player.java",
    "concepts/javabip/casinoBroken/Casino.java",
    "concepts/javabip/casinoBroken/Operator.java",
  )

  passingTest("concepts/javabip/casinoAdjusted/casinoAdjusted.json",
    "concepts/javabip/casinoAdjusted/Main.java",
    "concepts/javabip/casinoAdjusted/Constants.java",
    "concepts/javabip/casinoAdjusted/Player.java",
    "concepts/javabip/casinoAdjusted/Casino.java",
    "concepts/javabip/casinoAdjusted/Operator.java",
  )
}
