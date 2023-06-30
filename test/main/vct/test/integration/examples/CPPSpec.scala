package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class CPPSpec extends VercorsSpec {
  vercors should verify using silicon example("concepts/cpp/Arrays.cpp")
  vercors should verify using silicon example("concepts/cpp/Conditionals.cpp")
  vercors should verify using silicon example("concepts/cpp/Loops.cpp")
  vercors should verify using silicon example("concepts/cpp/Operators.cpp")
  vercors should verify using silicon examples("concepts/cpp/Pointers.cpp")
  vercors should verify using silicon examples("concepts/cpp/Scoping.cpp")
  vercors should verify using silicon examples("concepts/cpp/Types.cpp")

  vercors should verify using silicon examples("concepts/cpp/methods/AbstractMethod.cpp")
  vercors should verify using silicon examples("concepts/cpp/methods/ContextAndContextEverywhere.cpp")
  vercors should verify using silicon examples("concepts/cpp/methods/Decreases.cpp")
  vercors should verify using silicon examples("concepts/cpp/methods/GhostMethodsAndVariables.cpp")
  vercors should verify using silicon examples("concepts/cpp/methods/GhostParamsAndResults.cpp")
  vercors should verify using silicon examples("concepts/cpp/methods/InlineFunction.cpp")
  vercors should verify using silicon examples("concepts/cpp/methods/Permissions.cpp")
  vercors should verify using silicon examples("concepts/cpp/methods/Predicates.cpp")
  vercors should verify using silicon examples("concepts/cpp/methods/PureGhostMethod.cpp")
}