package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewritten}

case object ImportMap extends ImportADTBuilder("map")

case class ImportMap[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  private lazy val mapFile = parse("map")

  private lazy val mapAdt = find[AxiomaticDataType[Post]](mapFile, "map")
  private lazy val mapItems = find[ADTFunction[Post]](mapAdt, "map_items")
  private lazy val mapDomain = find[ADTFunction[Post]](mapAdt, "map_domain")
  private lazy val mapValues = find[ADTFunction[Post]](mapAdt, "map_values")
  private lazy val mapSize = find[ADTFunction[Post]](mapAdt, "map_size")
  private lazy val mapDisjoint = find[ADTFunction[Post]](mapAdt, "map_disjoint")
  private lazy val mapEqual = find[ADTFunction[Post]](mapAdt, "map_equal")
  private lazy val mapApply = find[ADTFunction[Post]](mapAdt, "map_apply")
  private lazy val mapEmpty = find[ADTFunction[Post]](mapAdt, "map_empty")
  private lazy val mapUpdate = find[ADTFunction[Post]](mapAdt, "map_update")
  private lazy val mapRemove = find[ADTFunction[Post]](mapAdt, "map_remove")

  private def mapTypeArgs(map: Expr[Pre]): Seq[Type[Post]] = {
    val mapType: TMap[Pre] = map.t.asMap.get
    Seq(dispatch(mapType.key), dispatch(mapType.value))
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TMap(k, v) => TAxiomatic(mapAdt.ref, Seq(dispatch(k), dispatch(v)))
    case other => rewriteDefault(other)
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = e match {
    case LiteralMap(keyType, valueType, values) =>
      implicit val o: Origin = e.o
      // the right-most values end up in the outer updates, and hence take precedence -> foldLeft
      values.foldLeft(
        ADTFunctionInvocation[Post](
          typeArgs = Some((mapAdt.ref, Seq(dispatch(keyType), dispatch(valueType)))),
          ref = mapEmpty.ref,
          args = Nil
        )
      ) {
        case (partialMap, (key, value)) =>
          ADTFunctionInvocation[Post](
            typeArgs = Some((mapAdt.ref, Seq(dispatch(keyType), dispatch(valueType)))),
            ref = mapUpdate.ref,
            Seq(partialMap, dispatch(key), dispatch(value))
          )
      }

    case MapCons(partialMap, key, value) =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(partialMap))),
        ref = mapUpdate.ref,
        Seq(dispatch(partialMap), dispatch(key), dispatch(value))
      )(e.o)

    case MapRemove(map, key) =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(map))),
        ref = mapRemove.ref,
        args = Seq(dispatch(map), dispatch(key)),
      )(e.o)

    case MapEq(left, right) =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(left))),
        ref = mapEqual.ref,
        Seq(dispatch(left), dispatch(right)),
      )(e.o)
    case MapDisjoint(left, right) =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(left))),
        ref = mapDisjoint.ref,
        Seq(dispatch(left), dispatch(right)),
      )(e.o)

    case MapKeySet(map) =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(map))),
        ref = mapDomain.ref,
        args = Seq(dispatch(map)),
      )(e.o)
    case MapValueSet(map) =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(map))),
        ref = mapValues.ref,
        args = Seq(dispatch(map)),
      )(e.o)
    case MapItemSet(map) =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(map))),
        ref = mapItems.ref,
        args = Seq(dispatch(map)),
      )(e.o)

    case MapMember(x, xs) =>
      SetMember(dispatch(x), ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(xs))),
        ref = mapDomain.ref,
        args = Seq(dispatch(xs)),
      )(e.o))(e.o)

    case MapGet(map, key) =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(map))),
        ref = mapApply.ref,
        args = Seq(dispatch(map), dispatch(key)),
      )(e.o)

    case Size(map) if map.t.asMap.nonEmpty =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((mapAdt.ref, mapTypeArgs(map))),
        ref = mapSize.ref,
        args = Seq(dispatch(map)),
      )(e.o)

    case other => rewriteDefault(other)
  }
}