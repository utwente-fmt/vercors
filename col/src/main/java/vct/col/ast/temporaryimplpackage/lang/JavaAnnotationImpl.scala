package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{Expr, JavaAnnotation}

trait JavaAnnotationImpl[G] { this: JavaAnnotation[G] =>
  def getArg(n: String): Option[Expr[G]] = args.find(_._1 == n).map(_._2)
}