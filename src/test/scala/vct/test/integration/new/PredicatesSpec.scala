package integration.`new`

import vct.test.integration.helper.VercorsSpec

class PredicatesSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/predicates/IntegerList.java"
  vercors should verify using silicon example "concepts/predicates/minmax-list.pvl"
  vercors should verify using silicon example "concepts/predicates/MutuallyRecursiveInlinePredicates.pvl"
  vercors should verify using silicon example "concepts/predicates/RecursiveInlinePredicate.pvl"
  vercors should verify using silicon example "concepts/predicates/ScaleInlinePredicate.pvl"
  vercors should verify using silicon example "concepts/predicates/TreeRecursive.java"
}
