package vct.col.failure

import vct.col.ast.Node

sealed trait PotentialFailure {
  def node: Node[_]
  def baseCode: String
}

/**
 * This failure is boolean, because it either occurs or it does not - it does not make sense to further subdivide the
 * failure logically. Of course more context can be added to it anyway, such as branch information, a counterexample,
 * etc.
 */
abstract class PotentialBooleanFailure(val baseCode: String) extends PotentialFailure

/**
 * This failure either does not occur, or does occur, and the reason is that an assertion cannot be verified in a
 * certain context. It then makes sense to further subdivide the failure to point to a more specific part of the
 * (potentially) false assertion.
 */
abstract class PotentialAssertionFailure(val baseCode: String) extends PotentialFailure