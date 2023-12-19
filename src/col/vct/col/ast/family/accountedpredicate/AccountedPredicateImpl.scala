package vct.col.ast.family.accountedpredicate

import vct.col.ast.AccountedPredicate

/**
 * A tree of assertions that represents the separating conjunction of the flattened tree. Either the tree splits
 * ([[vct.col.ast.SplitAccountedPredicate]]) or it is a leaf ([[vct.col.ast.UnitAccountedPredicate]]) consisting of one
 * expression. If verification of the predicate as a whole fails, additionally a path is returned that indicates the
 * position in the tree that failed via a path. See also [[vct.col.origin.AccountedDirection]],
 * [[vct.col.origin.PostBlameSplit]], [[vct.col.origin.PreBlameSplit]].
 */
trait AccountedPredicateImpl[G] { this: AccountedPredicate[G] =>

}
