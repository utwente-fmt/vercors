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

    def transSupports(node: Name, seen: List[Name] = Nil): ListSet[Name] = {
      if(seen.contains(node)) fail(s"Inheritance cycle: $seen")
      else
        _transSupports.getOrElseUpdate(node,
          ListSet(node) ++
            lookup
              .getOrElse(node, fail(s"Node $node is mentioned in the declarations, but cannot be found."))
              .supports
              .collect { case Type.Node(name) => name }
              .flatMap(transSupports(_, node +: seen))
              .to(ListSet)
        )
    }

    val declarationFamilies = decls.filter(decl => decl.family && transSupports(decl.name).contains(declaration.name))
    val structuralFamilies = decls.filter(decl => decl.family && transSupports(decl.name).contains(nodeFamily.name))

    val overlap = declarationFamilies.intersect(structuralFamilies)
    overlap.map(x => Try(x.blame) {
      fail(x.blame, "This category extends both NodeFamily and Declaration, which is not allowed.")
    }).get

    def validateType(t: Type, blame: Tree): Type = {
      def validateTypeRec(t: Type): Type =
        t match {
          case Type.Declaration(name) => Type.Declaration(name)
          case Type.DeclarationSeq(name) => Type.DeclarationSeq(name)
          case Type.Node(name) =>
            if (transSupports(name).contains(declaration.name))
              fail(blame, s"Declaration types (here: ${name.parts.mkString(".")} may only occur in field types directly, or as a sequence of that type.")
            Type.Node(name)
          case Type.Ref(node) =>
            if (!transSupports(node.name).contains(declaration.name)) {
              fail(blame, s"Category `${node.name.base}` cannot be referred to, because it is not part of any declaration family.")
            }

            transSupports(node.name).toSeq.intersect(declarationFamilies.map(_.name)) match {
              case Nil => Type.MultiRef(node)
              case _ => Type.Ref(node)
            }
          case Type.MultiRef(node) => Type.MultiRef(node)
          case Type.Tuple(args) => Type.Tuple(args.map(validateTypeRec))
          case Type.Seq(arg) => Type.Seq(validateTypeRec(arg))
          case Type.Option(arg) => Type.Option(validateTypeRec(arg))
          case Type.Either(left, right) => Type.Either(validateTypeRec(left), validateTypeRec(right))
          case primitiveType: Type.PrimitiveType => primitiveType
        }

      validateTypeRec(t)
    }

    def validateTopType(t: Type, blame: Tree): Type = t match {
      case Type.Node(name) if transSupports(name).contains(declaration.name) =>
        Type.Declaration(name)
      case Type.Seq(Type.Node(name)) if transSupports(name).contains(declaration.name) =>
        Type.DeclarationSeq(name)
      case other => validateType(other, blame)
    }

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

            val scopes = defn.scopes.map { node =>
              Try("scope") {
                if (!declarationFamilies.exists(_.name == node.name))
                  fail(decl.blame, s"Nodes may only scope declaration families (here: ${node.name.parts.mkString(".")})")

                Type.Declaration(node.name)
              }
            }.get

            val fields = defn.fields.map(field => field.copy(_2 = validateTopType(field._2, decl.blame)))

            NodeDefinition(decl.name, rootKind, scopes, fields, defn.blameType)
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
