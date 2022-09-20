package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.rewrite.Generation

case object ImportMapCompat extends ImportADTBuilder("map_compat")

case class ImportMapCompat[Pre <: Generation](importer: ImportADTImporter) extends AImportADT(importer) {
  private lazy val mapCompatFile = parse("map_compat")

  private lazy val mapCompatAdt = find[AxiomaticDataType[Post]](mapCompatFile, "map_compat")
  private lazy val mapItems = find[ADTFunction[Post]](mapCompatAdt, "map_items")
  private lazy val mapDisjoint = find[ADTFunction[Post]](mapCompatAdt, "map_disjoint")
  private lazy val mapRemove = find[ADTFunction[Post]](mapCompatAdt, "map_remove")

  private def mapTypeArgs(map: Expr[Pre]): Seq[Type[Post]] = {
    val mapType = map.t.asMap.get
    Seq(dispatch(mapType.key), dispatch(mapType.value))
  }

  override def dispatch(e: Expr[Nothing]): Expr[Post] = e match {
    case MapItemSet(map) =>
      ADTFunctionInvocation[Post](
        Some((mapCompatAdt.ref, mapTypeArgs(map))),
        mapItems.ref,
        Seq(dispatch(map))
      )
    case disj@MapDisjoint(left, right) =>
      ADTFunctionInvocation[Post](
        Some((mapCompatAdt.ref, Seq(dispatch(disj.commonMapType.key), dispatch(disj.commonMapType.value)))),
        mapDisjoint.ref,
        Seq(dispatch(left), dispatch(right)),
      )
    case MapRemove(map, k) =>
      ADTFunctionInvocation[Post](
        Some((mapCompatAdt.ref, mapTypeArgs(map))),
        mapRemove.ref,
        Seq(dispatch(map), dispatch(k)),
      )
  }
}
