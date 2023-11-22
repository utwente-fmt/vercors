package vct.col.ast.helpers.defn

import vct.col.ast.structure
import vct.col.ast.structure.NodeDefinition

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

  def compareTrait(node: NodeDefinition) = Type.Name(node.name.base + "Compare")
}
