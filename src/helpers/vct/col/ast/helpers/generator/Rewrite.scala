package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Naming._
import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Types
import vct.col.ast.structure
import vct.col.ast.structure.{NodeDefinition, NodeGenerator}

import java.nio.file.Path
import scala.meta._

class Rewrite extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition, isDeclaration: Boolean): Unit =
    ResultStream.write(out.resolve(s"${node.name.base}Rewrite"), getTree(node))

  def getTree(node: NodeDefinition): Tree =
    source"""
      package vct.col.ast.ops.rewrite

      trait ${rewriteTrait(node)}[Pre] { this: ${typ(node)}[Pre] =>
        def rewrite[Post](..${args(node)})(implicit `~rw`: $AbstractRewriter[Pre, Post]): ${typ(node)}[Post] =
          ${scopes(node, make(node))}
      }
    """

  def args(node: NodeDefinition): List[Term.Param] = {
    val fieldParams = node.fields.map {
      case (name, t) => arg(name, t)
    }

    val originParam = param"o: $Origin = null"

    (node.blameType match {
      case Some(blameType) =>
        fieldParams :+ blameArg(blameType) :+ originParam
      case None =>
        fieldParams :+ originParam
    }).toList
  }

  def arg(name: String, t: structure.Type): Term.Param =
    t match {
      case Types.ValueType() =>
        param"${Name(name)}: Option[${typ(t, t"Post")}] = None"
      case _ =>
        param"${Name(name)}: ${typ(t, t"Post")}= null"
    }

  def blameArg(blameType: structure.Name): Term.Param =
    param"blame: $Blame[${typ(blameType)}[G]] = null"

  def scopes(node: NodeDefinition, make: Term): Term = make

  def make(node: NodeDefinition): Term = {
    val fieldValues = node.fields.map {
      case (name, t) => makeField(name, t)
    }

    val valuess = node.blameType match {
      case Some(_) =>
        List(fieldValues.toList, List(q"if(blame ne null) blame else this.blame"), List(q"if(o ne null) o else this.o"))
      case None =>
        List(fieldValues.toList, List(q"o"))
    }

    q"new ${Init(typ(node.name), Name.Anonymous(), valuess)}"
  }

  def makeField(fieldName: String, t: structure.Type): Term = {
    val field = Term.Name(fieldName)
    if(Types.isValueType(t))
      q"$field.getOrElse(${rewriteDefault(q"this.$field", t)})"
    else
      q"if($field ne null) $field else ${rewriteDefault(q"this.$field", t)}"
  }

  def rewriteDefault(term: Term, t: structure.Type): Term =
    t match {
      case structure.Type.Node(_) => q"$term.rewriteDefault()(`~rw`)"
      case structure.Type.Ref(kind) => q"`~rw`.succ($term)"
      case Types.PrimitiveType() => term
      case Types.OptionType(t) => q"$term.map(`~x` => ${rewriteDefault(q"`~x`", t)})"
      case Types.EitherType(l, r) =>
        q"$term.fold(`~x` => $LeftObj(${rewriteDefault(q"`~x`", l)}), `~x` => $RightObj(${rewriteDefault(q"`~x`", r)}))"
      case Types.SeqType(t) => q"$term.map(`~x` => ${rewriteDefault(q"`~x`", t)})"
      case Types.TupleTypes(ts) =>
        val elems = ts.zipWithIndex.map {
          case (t, i) =>
            val field = Term.Name(s"_${i+1}")
            rewriteDefault(q"$term.$field", t)
        }.toList

        q"(..$elems)"
      case _ => q"???"
    }
}
