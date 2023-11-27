package vct.col.ast.analysis

import vct.col.ast.structure._
import Util._
import scala.meta.{Name => ScName, Type => ScType, _}
import scala.reflect.ClassTag

case class TypeAnalysis(typeLookup: Map[String, Seq[RawStatAnalysis.RawStat]], scope: Name) {
  def getTermName(t: Term): Result[Name] = Try(t) {
    t match {
      case Term.Name(name) => Name(Seq(name))
      case Term.Select(qual, name) => getTermName(qual).get + name.value
      case other => fail(other, "Expected a simple name here")
    }
  }

  def getName(t: ScType): Result[Name] = Try(t) {
    t match {
      case ScType.Name("G") => fail(t, "Expected a simple name here")
      case ScType.Name(name) => Name(Seq(name))
      case ScType.Select(qual, name) => getTermName(qual).get + name.value
      case other => fail(other, "Expected a simple name here")
    }
  }

  def get(t: ScType): Result[Type] = Try(t) {
    t match {
      case t"G" =>
        fail(t, "Generation parameter may not occur here")
      case _: ScType.Name | _: ScType.Select =>
        getPrimitive(getName(t).get, blame = t).get
      case ScType.Apply(t, List(t"G")) =>
        getNode(getName(t).get, blame = t).get
      case ScType.Apply(t, List(gen @ t"G", nodeType)) =>
        getRef(getName(t).get, get(nodeType).get, blame = t).get
          .getOrElse(fail(gen, "Generation parameter may not occur here"))
      case ScType.Apply(t, List(t0)) =>
        Constants.CollectionType1.getOrElse(getName(t).get.parts, fail(t, "This type is not supported"))(get(t0).get)
      case ScType.Apply(t, List(t0, t1)) =>
        Constants.CollectionType2.getOrElse(getName(t).get.parts, fail(t, "This type is not supported"))(get(t0).get, get(t1).get)
      case ScType.Tuple(ts) => Type.Tuple(ts.map(get).get)
      case other => fail(other, "This kind of type is not supported here")
    }
  }

  def getPrimitive(name: Name, blame: Tree): Result[Type] = Try(blame) {
    Constants.ValueTypes.get(name.parts)
      .orElse(Constants.PrimitiveTypes.get(name.parts))
      .getOrElse(fail(blame, "This type is not supported"))
  }

  def getRef(name: Name, nodeType: Type, blame: Tree): Result[Option[Type]] = Try("Ref type") {
    if (name != Constants.RefName && name != Constants.RefName.baseName)
      return Ok(None)

    val node = nodeType match {
      case n: Type.Node => n
      case _ => fail(blame, "The second type parameter of Ref must be a node category")
    }

    Some(Type.Ref(node))
  }

  def getNode(name: Name, blame: Tree): Result[Type.Node] = Try("Node type") {
    val message = "Unknown node type (Or: do not pass `G` as a type parameter to collections)"

    val candidates = typeLookup.getOrElse(name.base, fail(blame, message))
    val prefixLengths = (0 to scope.parts.size).reverse

    prefixLengths.foldLeft[Option[Type.Node]](None) {
      case (Some(result), _) => Some(result)
      case (None, prefixLength) =>
        val prefix = Name(scope.parts.take(prefixLength))
        candidates.filter(_.name == prefix + name) match {
          case Nil => None
          case Seq(rawStat) =>
            Some(Type.Node(prefix + name))
          case multiple =>
            fail(blame, s"Ambiguous reference to node type `${(name + prefix).parts.mkString(".")}`: Defined at: ${multiple.map(_.blame).map(posText).mkString(", ")}")
        }
    }.getOrElse(fail(blame, message))
  }
}