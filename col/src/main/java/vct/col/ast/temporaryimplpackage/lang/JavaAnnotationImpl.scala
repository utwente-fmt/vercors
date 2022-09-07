package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.temporaryimplpackage.lang.JavaAnnotationImpl.AnnotationElementMissing
import vct.col.ast.{Expr, JavaAnnotation, Type}
import vct.col.resolve.JavaAnnotationData
import vct.result.VerificationError.UserError

object JavaAnnotationImpl {
  case class AnnotationElementMissing(ann: JavaAnnotation[_], element: String) extends UserError {
    override def code: String = "annotationElementMissing"

    override def text: String = ann.o.messageInContext(s"The element $element should be present")
  }
}

object JavaAnnotationEx {
  def unapply[G](ja: JavaAnnotation[G]): Option[(Type[G], Seq[(String, Expr[G])], JavaAnnotationData[G])] = {
    ja.data match {
      case Some(data) => Some(ja.name, ja.args, data)
      case None => None
    }
  }
}

trait JavaAnnotationImpl[G] { this: JavaAnnotation[G] =>
  def get(n: String): Option[Expr[G]] = args.find(_._1 == n).map(_._2)

  def expect(elem: String): Expr[G] = get(elem) match {
    case Some(expr) => expr
    case None => throw AnnotationElementMissing(this, elem)
  }
}