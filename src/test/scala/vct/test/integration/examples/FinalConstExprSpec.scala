package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class FinalConstExprSpec extends VercorsSpec{

  vercors should verify using anyBackend examples("concepts/final/finalIntegerExplicit.java", "concepts/final/finalIntegerViaMethodInvocation.java")

  vercors should verify using anyBackend example "concepts/final/finalIntegerInstanceFunctionError.java"

  vercors should verify using anyBackend example "concepts/final/finalIntegerNoDecreasesError.java"

  vercors should verify using anyBackend example "concepts/final/finalIntegerImpureError.java"

}
