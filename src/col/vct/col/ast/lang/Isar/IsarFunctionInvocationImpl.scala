package vct.col.ast.lang.Isar

import vct.col.ast.{
  IsarDataConstructor,
  IsarDatatypeCommand,
  IsarDefinitionCommand,
  IsarFunctionInvocation,
  IsarLiftDefinitionCommand,
  TIsarType,
  Type,
}
import vct.col.ast.ops.IsarFunctionInvocationOps
import vct.col.print._

trait IsarFunctionInvocationImpl[G] extends IsarFunctionInvocationOps[G] {
  this: IsarFunctionInvocation[G] =>
  override def t: Type[G] =
    ref.decl match {
      case d: IsarDefinitionCommand[G] =>
        if (typeArgs.isEmpty)
          d.returnType
        else
          d.returnType.particularize(d.typevars.zip(typeArgs).toMap)
      case c: IsarDataConstructor[G] =>
        if (typeArgs.isEmpty)
          c.signature.last
        else
          c.signature.last.particularize(
            c.signature.last.asInstanceOf[TIsarType[G]].adt.decl
              .asInstanceOf[IsarDatatypeCommand[G]].typevars.zip(typeArgs).toMap
          )
      case l: IsarLiftDefinitionCommand[G] =>
        if (typeArgs.isEmpty)
          l.signature.last
        else
          l.signature.last.particularize(l.typevars.zip(typeArgs).toMap)
    }
  def layout_isar_function_name(implicit ctx: Ctx): Doc =
    Text(this.ref.decl match {
      case c: IsarDataConstructor[G] => c.name
      case d: IsarDefinitionCommand[G] => d.name
      case l: IsarLiftDefinitionCommand[G] => l.name
    })

  override def layout(implicit ctx: Ctx): Doc =
    Group((if (args.nonEmpty)
             Text("(") <> layout_isar_function_name <+> Doc.spread(args) <> ")"
           else
             layout_isar_function_name))
}
