package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Naming._
import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.structure
import vct.col.ast.structure.{NodeDefinition, NodeGenerator}

import java.nio.file.Path
import scala.meta._

class Rewrite extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit =
    ResultStream.write(out.resolve(s"${node.name.base}Rewrite.scala"), getTree(node))

  def getTree(node: NodeDefinition): Tree =
    source"""
      package $RewritePackage

      trait ${rewriteTrait(node)}[Pre] { this: ${typ(node)}[Pre] =>
        def rewriteDefault[Post]()(implicit `~rw`: $AbstractRewriter[Pre, Post]): ${typ(node)}[Post] =
          rewrite()(`~rw`)

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
      case _: structure.Type.ValueType =>
        param"${Name(name)}: Option[${typ(t, t"Post")}] = None"
      case _ =>
        param"${Name(name)}: ${typ(t, t"Post")} = null"
    }

  def blameArg(blameType: structure.Name): Term.Param =
    param"blame: $Blame[${typ(blameType)}] = null"

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
    if(t.isInstanceOf[structure.Type.ValueType])
      q"$field.getOrElse(${rewriteDefault(q"this.$field", t)})"
    else
      q"if($field ne null) $field else ${rewriteDefault(q"this.$field", t)}"
  }

  def rewriteDefault(term: Term, t: structure.Type): Term =
    t match {
      case structure.Type.Node(_) => q"$term.rewriteDefault()(`~rw`)"
      case structure.Type.Ref(kind) => q"`~rw`.succ[${typ(kind.name)}[Post]]($term.decl)"
      case structure.Type.MultiRef(kind) => q"`~rw`.anySucc[${typ(kind.name)}[Post]]($term.decl)"
      case _: structure.Type.PrimitiveType => term
      case structure.Type.Option(t) => q"$term.map(`~x` => ${rewriteDefault(q"`~x`", t)})"
      case structure.Type.Either(l, r) =>
        q"$term.fold(`~x` => $LeftObj(${rewriteDefault(q"`~x`", l)}), `~x` => $RightObj(${rewriteDefault(q"`~x`", r)}))"
      case structure.Type.Seq(t) => q"$term.map(`~x` => ${rewriteDefault(q"`~x`", t)})"
      case structure.Type.Tuple(ts) =>
        val elems = ts.zipWithIndex.map {
          case (t, i) =>
            val field = Term.Name(s"_${i+1}")
            rewriteDefault(q"$term.$field", t)
        }.toList

        q"(..$elems)"
    }
}
