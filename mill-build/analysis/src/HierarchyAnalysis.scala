package vct.col.ast.analysis

import vct.col.ast.structure._
import Util._

import scala.collection.immutable.{ListMap, ListSet}
import scala.collection.mutable
import scala.meta.{Name => ScName, Type => ScType, _}
import scala.reflect.ClassTag

object HierarchyAnalysis {
  case class Hierarchy(declaredNodes: Seq[(Name, Seq[NodeDefinition])], structuralNodes: Seq[(Name, Seq[NodeDefinition])])

  def get(decls: Seq[StatAnalysis.Decl]): Result[Hierarchy] = Try("hierarchy") {
    val rootNode =
      decls.find(_.name == Constants.RootNodeName)
        .getOrElse(fail(s"The node definitions do not contain a definition for the root type of all nodes, `${Constants.RootNodeName.parts.mkString(".")}`."))

    val nodeFamily =
      decls.find(_.name == Constants.NodeFamilyName)
        .getOrElse(fail(s"The node definitions do not contain a definition for the root type of all structural nodes, `${Constants.RootNodeName.parts.mkString(".")}`."))

    val declaration =
      decls.find(_.name == Constants.DeclarationName)
        .getOrElse(fail(s"The node definitions do not contain a definition for the root type of all declared nodes, `${Constants.RootNodeName.parts.mkString(".")}`."))

    val lookup = decls.map(x => x.name -> x).to(ListMap)

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

    val declarationFamilies = decls.filter(decl => decl.family && transSupports(decl.name).contains(declaration.name))
    val structuralFamilies = decls.filter(decl => decl.family && transSupports(decl.name).contains(nodeFamily.name))

    val overlap = declarationFamilies.intersect(structuralFamilies)
    overlap.map(x => Try(x.blame) {
      fail(x.blame, "This category extends both NodeFamily and Declaration, which is not allowed.")
    }).get

    val familyRoots = (declarationFamilies ++ structuralFamilies).map(_.name).to(ListSet)

    val defnsByRoots =
      decls
        .filter(_.defn.isDefined)
        .groupBy(decl => transSupports(decl.name).intersect(familyRoots))

    val defnsByRoot = defnsByRoots.map {
      case (roots, defns) =>
        Try(defns.head.blame) {
          if(roots.size == 1) roots.head -> defns.map(decl => {
            val defn = decl.defn.get
            val rootKind =
              if(declarationFamilies.exists(_.name == roots.head)) DeclaredNode
              else StructuralNode

            NodeDefinition(decl.name, defn.fields, defn.blameType, rootKind)
          })
          else if(roots.isEmpty)
            fail(defns.head.blame, "Node definitions must extends a node family or declaration kind")
          else
            fail(defns.head.blame, s"Node definitions must not extend more than one node family and/or declaration kind (Currently: ${roots.map(_.parts.mkString(".")).mkString(", ")})")
        }
    }.toSeq.get.to(ListMap)

    val declarations = declarationFamilies.map { family =>
      Try(family.blame) {
        family.name ->
          defnsByRoot.getOrElse(family.name, fail(family.blame, "Declaration kinds may not be empty"))
      }
    }

    val structuralNodes = structuralFamilies.map { family =>
      Try(family.blame) {
        family.name ->
          defnsByRoot
            .getOrElse(family.name, fail(family.blame, "Node families may not be empty"))
      }
    }

    check(declarations.all, structuralNodes.all)

    Hierarchy(declarations.get, structuralNodes.get)
  }
}
