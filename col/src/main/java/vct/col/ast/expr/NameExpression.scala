package vct.col.ast.expr

import vct.col.ast.`type`.ASTReserved
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.Hole
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor}

case class NameExpression(name: String, reserved: ASTReserved, var kind: NameExpressionKind) extends ASTNode {
  var site: ASTNode = null

  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A) = map.map(this, arg)

  override def accept_simple[T](visitor: ASTVisitor[T]) = visitor.visit(this)

  override def accept_simple[T](map: ASTMapping[T]) = map.map(this)

  override def debugTreeChildrenFields = Seq()

  override def debugTreePropertyFields = Seq("name", "kind", "reserved")

  def this(name: ASTReserved) =
    this(name.toString, name, NameExpressionKind.Reserved)

  /** Create an unresolved name expression */
  def this(name: String) =
    this(name, null, NameExpressionKind.Unresolved)

  /** Create an specific kind of name expression */
  def this(kind: NameExpressionKind, word: ASTReserved, name: String) =
    this(name, word, kind)

  def getKind: NameExpressionKind = kind

  def setKind(kind: NameExpressionKind): Unit = {
    if (kind eq NameExpressionKind.Reserved) hre.lang.System.Abort("cannot just declared a word reserved")
    this.kind = kind
  }

  def getSite: ASTNode = site

  def setSite(site: ASTNode): Unit = this.site = site

  def getName: String = name

  override def toString: String = name

  override def equals(o: Any): Boolean = o match {
    case other: NameExpression =>
      name == other.name
    case _ => false
  }

  override def isName(name: String): Boolean =
    this.name == name

  override def hashCode: Int = {
    if (name == null)
      hre.lang.System.Abort("name is null!")

    name.hashCode
  }

  override def isReserved(word: ASTReserved): Boolean = {
    if (word == null)
      return kind eq NameExpressionKind.Reserved

    reserved eq word
  }

  override def `match`(ast: ASTNode): Boolean =
    if (ast.isInstanceOf[Hole]) ast.`match`(this)
    else this == ast
}