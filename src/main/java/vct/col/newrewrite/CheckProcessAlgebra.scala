package vct.col.newrewrite

import vct.col.ast._

import scala.collection.mutable

case class CheckProcessAlgebra() extends Rewriter {

  override def dispatch(model: Declaration): Unit = model match {
//    case model: Model =>
////      val x = Function().declareDefault()
////      model.succeedDefault(this, x)
////      model.rewrite().declareDefault()
//
//      val compositeMap: mutable.Map[Seq[Expr], ModelProcess] = mutable.Map()
//
//      // This is still broken - but it compiles for now, so I can continue
//      model.declarations.flatMap {
//        case process: ModelProcess =>
//          process.impl match {
//            case processPar: ProcessPar =>
//              val parallelCompositionElems = processPar.unfoldProcessPar
//              if (parallelCompositionElems.forall(_.isInstanceOf[ProcessApply])) {
//                Some(parallelCompositionElems.permutations.map((_, process)))
//              } else {
//                None
//              }
//          }
//      }

//      model.declarations.foreach(dispatch(_))

    case process: ModelProcess =>
//      val modifiesRequires = process.modifies.map(Perm(_))
  }
}
