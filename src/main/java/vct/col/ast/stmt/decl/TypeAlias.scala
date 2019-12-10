package vct.col.ast.stmt.decl

import vct.col.ast.`type`.Type
import vct.col.ast.util.ASTVisitor
import vct.col.util.{ASTMapping, ASTMapping1}
import vct.util.ClassName

/**
  * A named alias for a type, used for e.g. typedef in C
  * @param aliasedType The type that is aliased
  * @param name The name of the type that is equivalent to the aliased type
  */
case class TypeAlias(aliasedType: Type, override val name: String) extends ASTDeclaration(name) {
  override def getDeclName: ClassName = new ClassName(name)

  override def accept_simple[T](visitor: ASTVisitor[T]): Unit = visitor.visit(this)
  override def accept_simple[T](map: ASTMapping[T]): T = map.map(this)
  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A): R = map.map(this, arg)

  override def debugTreeChildrenFields: Iterable[String] = Seq()
  override def debugTreePropertyFields: Iterable[String] = Seq("name", "aliasedType")
}
