package vct.test.integration.examples

import vct.test.integration.helper.JavaBipSpecHelper

class JavaBipSpec extends JavaBipSpecHelper {
  passingTest("concepts/javabip/casino/casino.json",
    "concepts/javabip/casino/Casino.java",
    "concepts/javabip/casino/Operator.java",
    "concepts/javabip/casino/Player.java",
    "concepts/javabip/casino/Constants.java",
    "concepts/javabip/casino/Main.java"
  )

  passingTest("concepts/javabip/deviation/deviation.json",
    "concepts/javabip/deviation/CalculatorSpec.java",
    "concepts/javabip/deviation/Constants.java",
    "concepts/javabip/deviation/DeviatorSpec.java",
    "concepts/javabip/deviation/GeneratorSpec.java",
    "concepts/javabip/deviation/Main.java",
  )
}
