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
      case ScType.Name("G") => Type.Generation
      case _: ScType.Name | _: ScType.Select => Type.Other(getName(t).get, Nil)
      case ScType.Apply(t, args0) =>
        val name = getName(t).get
        val args = args0.map(get).get
        getRef(name, args, blame = t).get
          .orElse(getNode(name, args, blame = t).get)
          .getOrElse(Type.Other(name, args))
      case ScType.Tuple(ts) => Type.Tuple(ts.map(get).get)
      case other => fail(other, "This kind of type is not supported here")
    }
  }

  def getRef(name: Name, args: Seq[Type], blame: Tree): Result[Option[Type]] = Try("Ref type") {
    if (name != Constants.RefName && name != Constants.RefName.baseName)
      return Ok(None)

    assertPure(args.size == 2, blame, "Ref must have two type arguments: `G` and a node category.").get

    val node = Try("Ref node type") {
      args.last match {
        case n: Type.Node => n
        case _ => fail(blame, "The second type parameter of Ref must be a node category")
      }
    }

    check(
      assertPure(args.head == Type.Generation, blame, "The first parameter of Ref must be `G`"),
      node,
    )

    Some(Type.Ref(node.get))
  }

  def getNode(name: Name, args: Seq[Type], blame: Tree): Result[Option[Type]] = Try("Node type") {
    val candidates = typeLookup.getOrElse(name.base, return Ok(None))
    val prefixLengths = (0 to scope.parts.size).reverse

    prefixLengths.foldLeft[Option[Type]](None) {
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
    }
  }
}