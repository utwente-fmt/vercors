package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming._
import vct.col.ast.helpers.defn.Simplify.simplify
import vct.col.ast.structure
import vct.col.ast.structure.{NodeDefinition, NodeGenerator, Type}

import java.nio.file.Path
import scala.meta._

class Subnodes extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit =
    ResultStream.write(out.resolve(s"${node.name.base}Subnodes.scala"), subnodes(node))

  def subnodes(node: NodeDefinition): Source =
    source"""
      package $SubnodesPackage

      trait ${subnodesTrait(node)}[G] { this: ${typ(node)}[G] =>
        def subnodes: $SeqType[$Node[G]] =
          ${simplify(subnodesFields(node))}
      }
    """

  def noNodes: Term =
    q"$SeqObj.empty[$Node[G]]"

  def concat(terms: Seq[Term]): Term =
    terms.reduceOption((l, r) => q"$l ++ $r").getOrElse(noNodes)

  def subnodesFields(node: NodeDefinition): Term =
    concat(node.fields.map {
      case (name, t) =>
        val term = q"this.${Term.Name(name)}"
        subnodes(term, t)
    })

  def subnodes(term: Term, typ: structure.Type): Term =
    typ match {
      case _: Type.Node =>
        q"$SeqObj($term)"
      case _: Type.Ref =>
        noNodes
      case Type.Tuple(ts) =>
        concat(ts.zipWithIndex.map {
          case (typ, i) =>
            val field = Term.Name(s"_${i+1}")
            subnodes(q"$term.$field", typ)
        })
      case Type.Seq(arg) =>
        q"$term.flatMap(`x` => ${subnodes(q"`x`", arg)})"
      case Type.Option(arg) =>
        q"if($term.isDefined) ${subnodes(q"$term.get", arg)} else $noNodes"
      case Type.Either(left, right) =>
        q"if($term.isLeft) ${subnodes(q"$term.left", left)} else ${subnodes(q"$term.right", right)}"
      case _: Type.PrimitiveType =>
        noNodes
    }
}
