package vct.col.compare

import vct.col.ast.{Declaration, Node}

sealed trait CompareResult[L, R]
case class MatchingDeclaration[L, R](
    left: Declaration[L],
    right: Declaration[R],
) extends CompareResult[L, R]
case class MatchingReference[L, R](left: Declaration[L], right: Declaration[R])
    extends CompareResult[L, R]
case class StructuralDifference[L, R](left: Node[L], right: Node[R])
    extends CompareResult[L, R]
