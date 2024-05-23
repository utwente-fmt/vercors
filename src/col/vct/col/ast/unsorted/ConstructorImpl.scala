package vct.col.ast.unsorted

import vct.col.ast.{Constructor, Statement, TClass, TVar, Variable}
import vct.col.ast.ops.ConstructorOps
import vct.col.print._

trait ConstructorImpl[G] extends ConstructorOps[G] {
  this: Constructor[G] =>
  override def pure: Boolean = false
  override def returnType: TClass[G] =
    TClass(cls, cls.decl.typeArgs.map((v: Variable[G]) => TVar(v.ref)))

  override def layout(implicit ctx: Ctx): Doc = {
    Doc.stack(Seq(
      contract,
      Group(
        Text("constructor") <> DocUtil.javaGenericParams(typeArgs) <> "(" <>
          Doc.args(args) <> ")"
      ) <> body.map(Text(" ") <> _).getOrElse(Text(";")),
    ))
  }

}
