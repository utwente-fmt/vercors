package vct.col.rewrite.adt

import vct.col.ast.RewriteHelpers.RewriteReturn
import vct.col.ast._
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers._

case object ImportVoid extends ImportADTBuilder("void")

case class ImportVoid[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  private lazy val voidFile = parse("void")

  private lazy val voidAdt = find[AxiomaticDataType[Post]](voidFile, "void")
  private lazy val voidUnit = find[ADTFunction[Post]](voidAdt, "unit")

  override def postCoerce(t: Type[Pre]): Type[Post] = t match {
    case TVoid() => TAxiomatic(voidAdt.ref, Nil)
    case other => rewriteDefault(other)
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = e match {
    case Void() =>
      ADTFunctionInvocation[Post](None, voidUnit.ref, Nil)(e.o)
    case other => rewriteDefault(other)
  }

  // PB: dumb hack alert: TVoid and Return(Void()) is (for viper) a marker to indicate that there is no return type.
  override def postCoerce(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] if method.returnType == TVoid[Pre]() =>
      allScopes.anyDeclare(allScopes.anySucceedOnly(method, method.rewrite(returnType = TVoid())))
    case other => super.postCoerce(other)
  }

  override def postCoerce(stat: Statement[Pre]): Statement[Post] = stat match {
    case ret@Return(v@Void()) => ret.rewrite(result = Void()(v.o))
    case other => rewriteDefault(other)
  }
}

