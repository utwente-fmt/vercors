package vct.col.ast.expr.op.cmp

import vct.col.ast.{AmbiguousComparison, TBool, TVector, Type}

trait AmbiguousComparisonImpl[G] { this: AmbiguousComparison[G] =>
  // So this is hacky, but in C languages the type of a vector comparison is vector<Int>, and here we need to make difference
  // Between CInt and Int because of the internal VerCors type checker.
  def vectorInnerType: Type[G]

  override def t: Type[G] = if(isVectorOp) TVector[G](getVectorType.size, vectorInnerType) else TBool()
}
