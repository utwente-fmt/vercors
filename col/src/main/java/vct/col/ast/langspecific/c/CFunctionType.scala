package vct.col.ast.langspecific.c

import vct.col.ast.`type`.Type
import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, TypeMapping}

import scala.jdk.CollectionConverters._

case class ParamSpec(t: Option[Type], name: Option[String]) {
  def asDecl: Option[DeclarationStatement] = (t, name) match {
    case (Some(t), Some(name)) => Some(new DeclarationStatement(name, t))
    case _ => None
  }

}

/**
 * A type representing a function type in the C frontend. The difference to the regular `FunctionType` is the addition
 * of parameter names. These are needed, because regular functions ("int add(int x, int y) { return x + y; }") and
 * function type declarations ("int (*f)(int, int);") share the same declaration type. The declaration also allows you
 * to specify the names of parameters, but to no effect.
 *
 * Furthermore, both the types of parameters and the names of parameters are optional. In fact, the only requirement
 * is that you specify one of the two, and function implementations must have at least the parameter name.
 *
 * @param params     the names and/or types to the function
 * @param returnType the return type of the function (possibly void)
 */
case class CFunctionType(params: Seq[ParamSpec], returnType: Type) extends Type {
  override def supertypeof(unit: ProgramUnit, t: Type): Boolean = t match {
    case funcType: CFunctionType => funcType == this
    case _ => false
  }

  def paramsJava: java.util.List[ParamSpec] = params.asJava

  override def accept_simple[T](visitor: ASTVisitor[T]): Unit = visitor.visit(this)

  override def accept_simple[T](map: ASTMapping[T]): T = map.map(this)

  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A): R = map.map(this, arg)

  override def debugTreeChildrenFields: Iterable[String] = Seq()

  override def debugTreePropertyFields: Iterable[String] = Seq("params", "returnType")

  override protected def accept_simple[T](map: TypeMapping[T]): T = map.map(this)
}
