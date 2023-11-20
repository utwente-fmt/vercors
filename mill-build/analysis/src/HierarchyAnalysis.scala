package vct.col.ast.analysis

import vct.col.ast.structure._
import Util._

import scala.collection.immutable.{ListMap, ListSet}
import scala.collection.mutable
import scala.meta.{Name => ScName, Type => ScType, _}
import scala.reflect.ClassTag

object HierarchyAnalysis {
  case class Hierarchy(declarations: Seq[(Name, Seq[NodeDefinition])], nodes: Seq[(Name, Seq[NodeDefinition])])

  def get(decls: Seq[(Tree, AnyNodeDeclaration)]): Result[Hierarchy] = Try("hierarchy") {
    val rootNode =
      decls.find(_._2.name == Constants.RootNodeName)
        .getOrElse(fail(s"The node definitions do not contain a definition for the root type of all nodes, `${Constants.RootNodeName.parts.mkString(".")}`."))

    val nodeFamily =
      decls.find(_._2.name == Constants.NodeFamilyName)
        .getOrElse(fail(s"The node definitions do not contain a definition for the root type of all nodes, `${Constants.RootNodeName.parts.mkString(".")}`."))
        ._2

    val declaration =
      decls.find(_._2.name == Constants.DeclarationName)
        .getOrElse(fail(s"The node definitions do not contain a definition for the root type of all nodes, `${Constants.RootNodeName.parts.mkString(".")}`."))
        ._2

    val declarationKinds = decls.filter(_._2.supports.contains(Type.Node(Constants.DeclarationName)))
    val nodeFamilies = decls.filter(_._2.supports.contains(Type.Node(Constants.NodeFamilyName)))

    val overlap = declarationKinds.intersect(nodeFamilies)
    overlap.map(x => Try(x._1) { fail(x._1, "This category extends both NodeFamily and Declaration, which is not allowed.") }).get

    val lookup = decls.map(x => x._2.name -> x._2).to(ListMap)

    val _transSupports: mutable.Map[Name, ListSet[Name]] = mutable.Map()

    def transSupports(node: Name): ListSet[Name] =
      _transSupports.getOrElseUpdate(node,
        ListSet(node) ++
          lookup
            .getOrElse(node, fail(s"Node $node is mentioned in the declarations, but cannot be found."))
            .supports
            .collect { case Type.Node(name) => name }
            .flatMap(transSupports)
            .to(ListSet)
      )

    val categoryRoots = (declarationKinds ++ nodeFamilies).map(_._2.name).to(ListSet)

    val defnsByRoots =
      decls
        .collect { case blame -> (defn: NodeDefinition) => blame -> defn }
        .groupBy { case _ -> defn => transSupports(defn.name).intersect(categoryRoots) }

    val defnsByRoot = defnsByRoots.map {
      case (roots, defns) =>
        Try(defns.head._1) {
          if(roots.size == 1) roots.head -> defns
          else if(roots.isEmpty)
            fail(defns.head._1, "Node definitions must extends a node family or declaration kind")
          else
            fail(defns.head._1, s"Node definitions must not extend more than one node family and/or declaration kind (Currently: ${roots.map(_.parts.mkString(".")).mkString(", ")})")
        }
    }.toSeq.get.to(ListMap)

    val declarations = declarationKinds.map {
      case blame -> kind =>
        Try(blame) {
          kind.name ->
            defnsByRoot
              .getOrElse(kind.name, fail(blame, "Declaration kinds may not be empty"))
              .map(_._2)
        }
    }

    val nodes = nodeFamilies.map {
      case blame -> family =>
        Try(blame) {
          family.name ->
            defnsByRoot
              .getOrElse(family.name, fail(blame, "Node families may not be empty"))
              .map(_._2)
        }
    }

    check(declarations.all, nodes.all)

    Hierarchy(declarations.get, nodes.get)
  }
}
