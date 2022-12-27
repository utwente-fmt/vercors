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

  // TODO (RR): Make this test sensible, and then make it passing, or delete it
  // failingTest("concepts/javabip/deviation/deviation.json",
  //   "concepts/javabip/deviation/CalculatorSpec.java",
  //   "concepts/javabip/deviation/Constants.java",
  //   "concepts/javabip/deviation/DeviatorSpec.java",
  //   "concepts/javabip/deviation/GeneratorSpec.java",
  //   "concepts/javabip/deviation/Main.java",
  // )
}
