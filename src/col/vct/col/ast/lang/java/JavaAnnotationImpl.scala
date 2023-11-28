package vct.col.ast.lang.java

import vct.col.ast.lang.java.JavaAnnotationImpl.AnnotationElementMissing
import vct.col.ast.{Expr, JavaAnnotation, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.resolve.lang.JavaAnnotationData
import vct.result.VerificationError.UserError
import vct.col.ast.ops.JavaAnnotationOps

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

trait JavaAnnotationImpl[G] extends JavaAnnotationOps[G] { this: JavaAnnotation[G] =>
  def get(n: String): Option[Expr[G]] = args.find(_._1 == n).map(_._2)

  def expect(elem: String): Expr[G] = get(elem) match {
    case Some(expr) => expr
    case None => throw AnnotationElementMissing(this, elem)
  }

  override def layout(implicit ctx: Ctx): Doc =
    Text("@") <> name <> "(" <> Doc.args(args.map { case (key, value) => Text(key) <> "=" <> value }) <> ")"
}