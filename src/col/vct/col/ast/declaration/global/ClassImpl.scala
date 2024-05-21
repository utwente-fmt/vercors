package vct.col.ast.declaration.global

import vct.col.ast.{Class, Declaration, TClass, TVar}
import vct.col.ast.util.Declarator
import vct.col.origin.{JavaLibrary, PositionRange, ReadableOrigin, SourceName}
import vct.col.print._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers.tt
import vct.result.VerificationError.Unreachable
import vct.col.ast.ops.ClassOps
import vct.col.resolve.lang.Java.JRESource

import scala.util.matching.Regex

trait ClassImpl[G] extends Declarator[G] with ClassOps[G] { this: Class[G] =>
  def transSupportArrowsHelper(seen: Set[TClass[G]]): Seq[(TClass[G], TClass[G])] = {
    val t: TClass[G] = TClass(this.ref, typeArgs.map(v => TVar(v.ref)))
    if(seen.contains(t)) Nil
    else supers.map(sup => (t, sup)) ++
      supers.flatMap(sup => sup.transSupportArrowsHelper(Set(t) ++ seen))
  }

  def transSupportArrows: Seq[(TClass[G], TClass[G])] = transSupportArrowsHelper(Set.empty)

  def supers: Seq[TClass[G]] = supports.map(_.asClass.get)

  override def declarations: Seq[Declaration[G]] = decls ++ typeArgs

  def layoutLockInvariant(implicit ctx: Ctx): Doc =
    Text("lock_invariant") <+> intrinsicLockInvariant <> ";" <+/> Empty

  private def searchInClassDeclaration(regex: Regex): Doc = {
    val readableOrigin = this.o.getReadable
    readableOrigin match {
      case Some(ro: ReadableOrigin) =>
        val rangeContent = o.get[PositionRange]
        val range = rangeContent.startLineIdx to rangeContent.endLineIdx
        val res = range.collectFirst {
          case index: Int if regex.findFirstMatchIn(ro.readable.readLines()(index)).nonEmpty => Text(regex.findFirstMatchIn(ro.readable.readLines()(index)).get.toString())
        }
        res.getOrElse(Empty)
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
        Some(this.supports.collect {
          case TClass(ref @ Ref(cls), _) if names.contains(cls.o.find[SourceName].map(_.name).getOrElse("~")) => ref
        })
      case _ => None
    }
  }

  def extendsRef(): Option[Ref[G, Class[G]]] = {
    this.extendsDoc() match {
      case Text(s) =>
        val name = s.replaceAll("extends\\s+","")
        this.supports.collectFirst {
          case TClass(ref @ Ref(cls), _) if cls.o.find[SourceName].map(_.name).getOrElse("~") == name => ref
        }
      case _ => None
    }
  }

  def fromLibrary(): Boolean =
    o.find[JavaLibrary.type].nonEmpty || o.find[JRESource.type].nonEmpty

  override def layout(implicit ctx: Ctx): Doc = {
    val classOrInterface = classOrInterfaceDoc() match {
      case Empty => Text("class")
      case e => e
    }
    val extension = extendsDoc()
    val implements = implementsDoc()

    if (fromLibrary()) {
      Empty
    } else {
      (if(intrinsicLockInvariant == tt[G]) Empty else Doc.spec(Show.lazily(layoutLockInvariant(_)))) <>
        Group(
          Text("class") <+> ctx.name(this) <>
            (if (typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs) <> ">" else Empty) <>
            (if(supports.isEmpty) Empty else Text(" implements") <+>
              Doc.args(supports.map(supp => ctx.name(supp.asClass.get.cls)).map(Text))) <+>
            "{"
        ) <>>
        Doc.stack(decls) <+/>
        "}"
    }
  }
}