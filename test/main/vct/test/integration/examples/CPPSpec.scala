package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class CPPSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/cpp/Arrays.cpp"
  vercors should verify using silicon example "concepts/cpp/Conditionals.cpp"
  vercors should verify using silicon example "concepts/cpp/Loops.cpp"
  vercors should verify using silicon example "concepts/cpp/Namespaces.cpp"
  vercors should error withCode "noSuchName" example "concepts/cpp/NamespacesDoNotFindWithoutNamespacePrefix.cpp"
  vercors should verify using silicon example "concepts/cpp/Operators.cpp"
  vercors should verify using silicon example "concepts/cpp/Pointers.cpp"
  vercors should verify using silicon example "concepts/cpp/Scoping.cpp"
  vercors should verify using silicon example "concepts/cpp/Types.cpp"

  vercors should verify using silicon example "concepts/cpp/methods/AbstractMethod.cpp"
  vercors should verify using silicon example "concepts/cpp/methods/ContextAndContextEverywhere.cpp"
  vercors should verify using silicon example "concepts/cpp/methods/Decreases.cpp"
  vercors should verify using silicon example "concepts/cpp/methods/GhostMethodsAndVariables.cpp"
  vercors should verify using silicon example "concepts/cpp/methods/GhostParamsAndResults.cpp"
  vercors should verify using silicon example "concepts/cpp/methods/InlineFunction.cpp"
  vercors should verify using silicon example "concepts/cpp/methods/Overloading.cpp"
  vercors should verify using silicon example "concepts/cpp/methods/Permissions.cpp"
  vercors should verify using silicon example "concepts/cpp/methods/Predicates.cpp"
  vercors should verify using silicon example "concepts/cpp/methods/PureGhostMethod.cpp"

  vercors should error withCode "preprocessorError" in "Source file with preprocessor error" cpp
  """
  #define foo(
  """

  vercors should verify using silicon in "Character literal" cpp
  """
  char a = 'a';
  char b = '\u0062';
  """

  vercors should error withCode "parseError" in "Multicharacter literal" cpp
  """
  char c = 'bad!';
  """

  vercors should error withCode "parseError" in "Unrepresentable character literal" cpp
  """
  char d = 'é';
  """
}