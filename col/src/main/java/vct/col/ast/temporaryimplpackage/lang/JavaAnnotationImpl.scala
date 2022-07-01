package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.temporaryimplpackage.lang.JavaAnnotationImpl.AnnotationElementMissing
import vct.col.ast.{Expr, JavaAnnotation}
import vct.result.VerificationError.UserError

object JavaAnnotationImpl {
  case class AnnotationElementMissing(ann: JavaAnnotation[_], element: String) extends UserError {
    override def code: String = "annotationElementMissing"

    override def text: String = ann.o.messageInContext(s"The element $element should be present")
  }
}

trait JavaAnnotationImpl[G] { this: JavaAnnotation[G] =>
  def get(n: String): Option[Expr[G]] = args.find(_._1 == n).map(_._2)

  def expect(elem: String): Expr[G] = get(elem) match {
    case Some(expr) => expr
    case None => throw AnnotationElementMissing(this, elem)
  }
}