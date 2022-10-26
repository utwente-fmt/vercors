package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class PredicatesSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/predicates/IntegerList.java"
  vercors should verify using silicon example "concepts/predicates/minmax-list.pvl"
  vercors should error withCode "cyclicInline" example "concepts/predicates/MutuallyRecursiveInlinePredicates.pvl"
  vercors should error withCode "cyclicInline" example "concepts/predicates/RecursiveInlinePredicate.pvl"
  vercors should verify using silicon example "concepts/predicates/ScaleInlinePredicate.pvl"
  // https://github.com/utwente-fmt/vercors/discussions/842
  // vercors should verify using silicon example "concepts/predicates/TreeRecursive.java"
}
