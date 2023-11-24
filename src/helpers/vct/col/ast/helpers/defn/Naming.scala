package vct.col.ast.helpers.defn

import vct.col.ast.helpers.defn.Constants.RefType
import vct.col.ast.structure
import vct.col.ast.structure.{NodeDefinition}

import scala.meta._

object Naming {
  def term(name: structure.Name): Term.Ref =
    name.parts.tail.foldLeft[Term.Ref](Term.Name(name.parts.head)) {
      case (term, part) => Term.Select(term, Term.Name(part))
    }

  def typ(name: structure.Name): Type =
    if(name.parts.size == 1) Type.Name(name.base)
    else Type.Select(term(name.initName), Type.Name(name.base))

  def typ(node: structure.AnyNodeDeclaration): Type = typ(node.name)

  def typ(t: structure.Type, g: Type): Type = t match {
    case structure.Type.Node(name) => t"${typ(name)}[$g]"
    case structure.Type.Ref(node) => t"$RefType[$g, ${typ(node.name)}[$g]]"
    case structure.Type.Generation => g
    case structure.Type.Tuple(args) => t"(..${args.toList.map(typ(_, g))})"
    case structure.Type.Other(name, Nil) => t"${typ(name)}"
    case structure.Type.Other(name, args) => t"${typ(name)}[..${args.toList.map(typ(_, g))}]"
  }

  def compareTrait(node: NodeDefinition) = Type.Name(node.name.base + "Compare")
  def rewriteTrait(node: NodeDefinition) = Type.Name(node.name.base + "Rewrite")
}
