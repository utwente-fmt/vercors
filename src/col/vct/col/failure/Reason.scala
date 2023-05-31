package vct.col.failure

import vct.col.ast.Expr

sealed trait Reason
case class AssertionFalse(e: Expr[_]) extends Reason
case class InsufficientPermissionToExhale(e: Expr[_]) extends Reason

/*
Since evicted:
- Negative permission amount
- Injectivity
*/