package vct.col.ast.lang.Isar

import hre.io.Writeable
import vct.col.print._

case object IsarDoc {
  def inner(block: => Doc)(implicit ctx: Ctx): Doc =
    Group(openInner <> block <> closeInner)
  def type_signature(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    Doc.foldr(docs)(_ <+> "⇒" <+> _)
  def alternative(docs: Iterable[Show])(implicit ctx: Ctx): Doc =
    Group(Doc.foldr(docs)(_ <+> "|" <+> _))

  def <::> = Text("::")

  // keywords
  def openInner: Doc = Text("\"")
  def closeInner: Doc = Text("\"")
  def ignore: Doc = Text("_")

  def begin: Doc = Text("begin")
  def end: Doc = Text("end")
  def where: Doc = Text("where")
}
