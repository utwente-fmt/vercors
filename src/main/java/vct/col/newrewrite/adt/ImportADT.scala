package vct.col.newrewrite.adt

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers._

case object ImportADT extends ImportADTBuilder("*") {
  case class MapKeyErrorPreconditionFailed(access: MapGet[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      access.blame.blame(MapKeyError(access))
  }
}

case class ImportADT[Pre <: Generation](importer: ImportADTImporter) extends AImportADT[Pre](importer) {

  override def applyCoercion(e: Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceNullRef() =>
      SilverNull()

    case _ => super.applyCoercion(e, coercion)
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    globalBlame.having(program.blame) {
      program.rewrite(declarations = globalDeclarations.collect {
        parse("viper_order")
        program.declarations.foreach(dispatch)
      }._1)
    }
  }

  // PB: dumb hack alert: TVoid and Return(Void()) is (for viper) a marker to indicate that there is no return type.
  override def postCoerce(decl: Declaration[Pre]): Unit = decl match {
    case method: AbstractMethod[Pre] if method.returnType == TVoid[Pre]() =>
      allScopes.anyDeclare(allScopes.anySucceedOnly(method, method.rewrite(returnType = TVoid())))
    case other => super.postCoerce(other)
  }

  override def postCoerce(stat: Statement[Pre]): Statement[Post] = stat match {
    case ret @ Return(v @ Void()) => ret.rewrite(result=Void()(v.o))
    case other => rewriteDefault(other)
  }
}
