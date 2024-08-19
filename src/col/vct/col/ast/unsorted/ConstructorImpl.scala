package vct.col.ast.unsorted

import vct.col.ast.{Constructor, Statement, TClass, TVar, Variable}
import vct.col.ast.ops.ConstructorOps
import vct.col.print._

trait ConstructorImpl[G] extends ConstructorOps[G] {
  this: Constructor[G] =>
  override def pure: Boolean = false
  override def returnType: TClass[G] =
    cls.decl.classType(cls.decl.typeArgs.map((v: Variable[G]) => TVar(v.ref)))

  def layoutJava(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Text(ctx.name(cls.decl)) <> DocUtil.javaGenericParams(typeArgs) <>
          "(" <> Doc.args(args) <> ")"
      ) <> body.map(Text(" ") <> _).getOrElse(Text(";")),
    ))

  def layoutPvl(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        Text("constructor") <> DocUtil.javaGenericParams(typeArgs) <> "(" <>
          Doc.args(args) <> ")"
      ) <> body.map(Text(" ") <> _).getOrElse(Text(";")),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Java => layoutJava
      case _ => layoutPvl
    }

}
