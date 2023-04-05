package vct.col.ast.expr.literal.constant

import vct.col.ast.{BooleanValue, TBool, Type}
import vct.col.print._

trait BooleanValueImpl[G] { this: BooleanValue[G] =>
  override def t: Type[G] = TBool()

  sealed trait LayoutElement[+T] { def textualData: Char }
  case class SharedLayoutElement(textualData: Char) extends LayoutElement[Nothing]
  case class DedicatedLayoutElement[T](resource: T, textualData: Char) extends LayoutElement[T]
  def orderedLayoutFixture: Seq[LayoutElement[Boolean]] = Seq(DedicatedLayoutElement(true, 't'),
    DedicatedLayoutElement(false, 'f'), DedicatedLayoutElement(false, 'a'), DedicatedLayoutElement(true, 'r'),
    DedicatedLayoutElement(true, 'u'), DedicatedLayoutElement(false, 'l'), DedicatedLayoutElement(false, 's'),
    SharedLayoutElement('e'))
  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text(orderedLayoutFixture.collect { case e@SharedLayoutElement(_) => e; case e@DedicatedLayoutElement(r, _) if r.toString == value.toString => e }.map(_.textualData).mkString(""))
}
