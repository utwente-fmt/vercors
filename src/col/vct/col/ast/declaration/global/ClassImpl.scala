package vct.col.ast.declaration.global

import vct.col.ast.Class
import vct.col.ast.util.Declarator
import vct.col.origin.{ReadableOrigin, ShortPosition}
import vct.col.print._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers.tt
import vct.result.VerificationError.Unreachable

import scala.util.matching.Regex

trait ClassImpl[G] extends Declarator[G] {
  this: Class[G] =>
  protected def transSupportArrows(seen: Set[Class[G]]): Seq[(Class[G], Class[G])] =
    if (seen.contains(this)) Nil
    else supports.map(other => (this, other.decl)) ++
      supports.flatMap(other => other.decl.transSupportArrows(Set(this) ++ seen))

  def transSupportArrows: Seq[(Class[G], Class[G])] = transSupportArrows(Set.empty)

  def layoutLockInvariant(implicit ctx: Ctx): Doc =
    Text("lock_invariant") <+> intrinsicLockInvariant <+/> Empty

  private def searchInClassDeclaration(regex: Regex): Doc = {
    val readableOrigin = this.o.getReadable
    readableOrigin match {
      case Some(ro: ReadableOrigin) => {
        val start = this.o.getStartEndLines.get.startEndLineIdx._1
        val classDeclaration = ro.readable.readLines()(start)
        regex.findFirstMatchIn(classDeclaration) match {
          case Some(matched) =>
            Text(matched.toString())
          case None => Empty
        }
      }
      case None => Empty
    }

  }

  def classOrInterfaceDoc(): Doc =
    searchInClassDeclaration(new Regex("(class|interface)"))

  def implementsDoc(): Doc =
    searchInClassDeclaration(new Regex("implements(\\s+[A-Z][A-Za-z]*,?)*"))

  def extendsDoc(): Doc =
    searchInClassDeclaration(new Regex("extends\\s+[A-Z][A-Za-z]*"))

  def implementsRef(): Option[Seq[Ref[G, Class[G]]]] = {
    this.implementsDoc() match {
      case Text(s) =>
        val names = s.replaceAll("implements\\s+", "").split(",\\s+")
        Some(this.supports.filter(supp => names.contains(supp.decl.o.getPreferredNameOrElse())))
      case _ => None
    }
  }

  def extendsRef(): Option[Ref[G, Class[G]]] = {
    this.extendsDoc() match {
      case Text(s) =>
        val name = s.replaceAll("extends\\s+","")
        this.supports.find(supp => supp.decl.o.getPreferredNameOrElse().equals(name))
      case _ => None
    }
  }

  def fromLibrary(): Boolean = {
    this.o.originContents.collectFirst{case sp: ShortPosition => sp}.nonEmpty
  }


  override def layout(implicit ctx: Ctx): Doc = {
    val classOrInterface = classOrInterfaceDoc()
    val extension = extendsDoc()
    val implements = implementsDoc()

    if (fromLibrary()) {
      Empty
    } else {
      (if (intrinsicLockInvariant == tt[G]) Empty else Doc.spec(Show.lazily(layoutLockInvariant(_)))) <>
        Group(
          classOrInterface <+> ctx.name(this) <+> extension <+> implements <+> "{"
        ) <>>
        Doc.stack(declarations) <+/>
        "}"
    }
  }
}