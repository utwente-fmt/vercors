package vct.clexer

sealed trait PpToken

case class HeaderName(data: String) extends PpToken
case class Identifier(data: String) extends PpToken
case class PpNumber(data: String) extends PpToken
case class CharacterConstant(data: String) extends PpToken
case class StringLiteral(data: String) extends PpToken
case class Punctuator(char: Char) extends PpToken

sealed trait ConstExpr

sealed trait DirectiveGroupPart
case class IfSection(branches: Seq[(ConstExpr, DirectiveGroupPart)], otherwise: Option[DirectiveGroupPart]) extends DirectiveGroupPart
case class Include(what: Seq[PpToken]) extends DirectiveGroupPart
case class Define(name: String, args: Seq[String], vaArgs: Boolean, replacement: Seq[PpToken]) extends DirectiveGroupPart
case class Undef(name: String) extends DirectiveGroupPart
case class Line(data: Seq[PpToken]) extends DirectiveGroupPart
case class Error(data: Seq[PpToken]) extends DirectiveGroupPart
case class Pragma(data: Seq[PpToken]) extends DirectiveGroupPart
case class NopDirective() extends DirectiveGroupPart
case class TextLine(data: Seq[PpToken])