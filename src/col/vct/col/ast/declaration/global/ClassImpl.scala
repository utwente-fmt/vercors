package vct.col.ast.declaration.global

import vct.col.ast.{Class, Declaration, TClass, TVar}
import vct.col.ast.util.Declarator
import vct.col.print._
import vct.col.util.AstBuildHelpers.tt
import vct.result.VerificationError.Unreachable
import vct.col.ast.ops.ClassOps

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
    Text("lock_invariant") <+> intrinsicLockInvariant <+/> Empty

  override def layout(implicit ctx: Ctx): Doc =
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