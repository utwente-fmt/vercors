package vct.test.integration.examples

import vct.test.integration.helper.JavaBipSpecHelper

class JavaBipSpec extends JavaBipSpecHelper {
  val base = "publications/2023/JavaBIP"
  failingTest(Seq(
      "bipComponentInvariantNotMaintained:false",
      "bipComponentInvariantNotMaintained:false",
      "bipStateInvariantNotMaintained:false",
      "bipStateInvariantNotMaintained:false",
      "bipComponentInvariantNotMaintained:false"
    ),
    s"$base/casinoBroken/casinoBroken.json",
    s"$base/casinoBroken/Main.java",
    s"$base/casinoBroken/Constants.java",
    s"$base/casinoBroken/Player.java",
    s"$base/casinoBroken/Casino.java",
    s"$base/casinoBroken/Operator.java",
  )

  passingTest(s"$base/casinoAdjusted/casinoAdjusted.json",
    s"$base/casinoAdjusted/Main.java",
    s"$base/casinoAdjusted/Constants.java",
    s"$base/casinoAdjusted/Player.java",
    s"$base/casinoAdjusted/Casino.java",
    s"$base/casinoAdjusted/Operator.java",
  )
}
