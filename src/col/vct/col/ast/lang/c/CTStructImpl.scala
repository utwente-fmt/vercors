package vct.col.ast.lang.c

import hre.util.ScopedStack
import vct.col.ast.{
  CDeclaration,
  CDeclarationSpecifier,
  CSpecificationType,
  CStructDeclaration,
  CStructMemberDeclarator,
  CTStruct,
  TUnique,
}
import vct.col.ast.ops.CTStructOps
import vct.col.origin.Origin
import vct.col.print.{Ctx, Doc, Text}
import vct.col.typerules.TypeSize
import vct.result.VerificationError.UserError

import scala.collection.mutable

trait CTStructImpl[G] extends CTStructOps[G] {
  this: CTStruct[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("struct") <+> ctx.name(ref)

  private val noRepeat: ScopedStack[mutable.HashSet[CTStruct[G]]] =
    ScopedStack()

  override def bits: TypeSize = {
    if (noRepeat.isEmpty) { noRepeat.having(mutable.HashSet()) { bits } }
    else {
      val set = noRepeat.top
      if (!set.add(this))
        throw CyclicStruct(ref.decl.o)
      val decls =
        ref.decl.decl match {
          case CDeclaration(
                _,
                Seq(CStructDeclaration(Some(_), decls)),
                Seq(),
              ) =>
            decls
          case _ => ???
        }
      val sizes = decls.map { fieldDecl =>
        val CStructMemberDeclarator(
          specs: Seq[CDeclarationSpecifier[G]],
          Seq(_),
        ) = fieldDecl
        specs.collectFirst { case t: CSpecificationType[G] =>
          t.t match {
            case TUnique(inner, _) => inner
            case inner => inner
          }
        }.get.bits
      }
      TypeSize.struct(sizes: _*)
    }
  }
}

private case class CyclicStruct(o: Origin) extends UserError {
  override def code: String = "cyclicStruct"

  override def text: String =
    o.messageInContext("This struct contains itself, this is not allowed")
}
