package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Naming._
import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming
import vct.col.ast.structure
import vct.col.ast.structure.{NodeDefinition, NodeGenerator}

import java.nio.file.Path
import scala.meta._

class Rewrite extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit =
    ResultStream
      .write(out.resolve(s"${node.name.base}Rewrite.scala"), getTree(node))

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
    val fieldParams = node.fields.map { case (name, t) => arg(name, t) }

    val originParam = param"o: $Origin = null"

    (node.blameType match {
      case Some(blameType) => fieldParams :+ blameArg(blameType) :+ originParam
      case None => fieldParams :+ originParam
    }).toList
  }

  def arg(name: String, t: structure.Type): Term.Param =
    t match {
      case _: structure.Type.ValueType =>
        param"${Name(name)}: => Option[${typ(t, t"Post")}] = None"
      case _ => param"${Name(name)}: => ${typ(t, t"Post")} = null"
    }

  def blameArg(blameType: structure.Name): Term.Param =
    param"blame: $Blame[${typ(blameType)}] = null"

  def scopes(node: NodeDefinition, make: Term): Term =
    node.scopes.foldLeft[Term](make) {
      case (acc, structure.Type.Declaration(name)) => q"""
          `~rw`.${Naming.scopes(name.base)}.scope {
            $acc
          }
        """
    }

  def make(node: NodeDefinition): Term = {

    val fieldResolvedDefs = node.fields.map { case (fieldName, t) =>
      val resolved = Term.Name(s"_resolved_$fieldName")
      val field = Term.Name(fieldName)
      q"val ${Pat.Var(resolved)} = ${makeField(fieldName, t)}"
    }

    val fieldResolvedTerms = node.fields.map { case (fieldName, _) =>
      val resolved = Term.Name(s"_resolved_$fieldName")
      q"$resolved"
    }

    val fieldReuseConditions = node.fields.map { case (fieldName, t) =>
      val resolved = Term.Name(s"_resolved_$fieldName")
      val original = q"this.${Term.Name(fieldName)}"
      val isOriginal = q"${unchanged(t, q"$resolved", original)}"
      t match {
        case _: structure.Type.ValueType =>
          q"${Term.Name(fieldName)}.isEmpty && $isOriginal"
        case _ => isOriginal
      }
    }

    val blameResolvedDef = node.blameType.map(_ =>
      q"val _resolved_blame = if(blame ne null) blame else `~rw`.dispatch(this.blame)"
    )

    val blameReuseCondition = node.blameType.map(_ =>
      q"(_resolved_blame.asInstanceOf[_root_.scala.AnyRef] eq this.blame.asInstanceOf[_root_.scala.AnyRef])"
    )

    val oResolvedDef: Stat =
      q"val _resolved_o = if(o ne null) o else `~rw`.dispatch(this.o)"
    val oReuseCondition =
      q"(_resolved_o.asInstanceOf[_root_.scala.AnyRef] eq this.o.asInstanceOf[_root_.scala.AnyRef])"
    val setupDefs: List[Stat] =
      ((fieldResolvedDefs) ++ blameResolvedDef.toSeq :+ oResolvedDef).toList

    val reuseSafeForNode =
      // (node.kind != structure.DeclaredNode) &&
      // node.name.base != "Program"
      true
    // node.scopes.isEmpty //&&
    //   !node.fields.exists { case (_, t) =>
    //     t match {
    //       // case structure.Type.Declaration(_) | structure.Type.DeclarationSeq(_) |
    //       //     structure.Type.Ref(_) | structure.Type.MultiRef(_) => true
    //       case _ => false
    //     }
    //   }

    val reuseConditions =
      fieldReuseConditions ++ blameReuseCondition.toSeq :+ oReuseCondition
    val baseReuseCondition = reuseConditions.reduce[Term] { (acc, cond) =>
      q"$acc && $cond"
    }
    val reuseCondition =
      q"${Lit.Boolean(reuseSafeForNode)} && $baseReuseCondition"

    val valuess =
      node.blameType match {
        case Some(_) =>
          List(
            fieldResolvedTerms.toList,
            List(q"_resolved_blame"),
            List(q"_resolved_o"),
          )
        case None => List(fieldResolvedTerms.toList, List(q"_resolved_o"))
      }

    val nodeNameLit = Lit.String(node.name.base)
    val reuse =
      if (node.kind == structure.DeclaredNode)
        q"{`~rw`.reuseDecl.add(this); this.asInstanceOf[${typ(node)}[Post]]}"
      else
        q"this.asInstanceOf[${typ(node)}[Post]]"
    q"{ ..$setupDefs; val _reuseOriginal = $reuseCondition; _root_.vct.col.rewrite.ReuseTracker.record($nodeNameLit, _reuseOriginal); if (_reuseOriginal) $reuse else new ${Init(typ(node.name), Name.Anonymous(), valuess)} }"
  }

  def makeField(fieldName: String, t: structure.Type): Term = {
    val field = Term.Name(fieldName)
    if (t.isInstanceOf[structure.Type.ValueType])
      q"$field.getOrElse(${rewriteDefault(q"this.$field", t)})"
    else
      q"_root_.scala.Predef.locally({ val `~x` = $field; if(`~x` ne null) `~x` else ${rewriteDefault(q"this.$field", t)} })"
  }

  def rewriteDefault(term: Term, t: structure.Type): Term =
    t match {
      case structure.Type.Node(_) => q"`~rw`.dispatch($term)"
      case structure.Type.Declaration(name) =>
        q"`~rw`.${Naming.scopes(name.base)}.dispatch($term)"
      case structure.Type.DeclarationSeq(name) =>
        q"`~rw`.${Naming.scopes(name.base)}.dispatch($term)"
      case structure.Type.Ref(kind) =>
        // q"""
        //   `~rw`.porcelainRefSucc[${typ(kind.name)}[Post]]($term).getOrElse {
        //     val _decl = $term.decl
        //     if(_decl.succeededSame) $term.asInstanceOf[_root_.vct.col.ref.Ref[Post, ${typ(kind.name)}[Post]]]
        //     else `~rw`.succ[${typ(kind.name)}[Post]](_decl)
        //   }
        //   """
        q"""
          `~rw`.porcelainRefSucc[${typ(kind.name)}[Post]]($term).getOrElse {
            val _decl = $term.decl
            if (`~rw`.reuseDecl.contains(_decl)) $term.asInstanceOf[_root_.vct.col.ref.Ref[Post, ${typ(kind.name)}[Post]]]
            else `~rw`.succ[${typ(kind.name)}[Post]](_decl)
          }
        """
      case structure.Type.MultiRef(kind) =>
        q"`~rw`.porcelainRefSucc[${typ(kind.name)}[Post]]($term).getOrElse(`~rw`.anySucc[${typ(kind.name)}[Post]]($term.decl))"
      case _: structure.Type.PrimitiveType => term
      case structure.Type.Option(t) =>
        q"$term.map(`~x` => ${rewriteDefault(q"`~x`", t)})"
      case structure.Type.Either(l, r) =>
        q"$term.fold(`~x` => $LeftObj(${rewriteDefault(q"`~x`", l)}), `~x` => $RightObj(${rewriteDefault(q"`~x`", r)}))"
      case structure.Type.Seq(t) =>
        q"$term.map(`~x` => ${rewriteDefault(q"`~x`", t)})"
      case structure.Type.Tuple(ts) =>
        val elems =
          ts.zipWithIndex.map { case (t, i) =>
            val field = Term.Name(s"_${i + 1}")
            rewriteDefault(q"$term.$field", t)
          }.toList

        q"(..$elems)"
    }

  def unchanged(t: structure.Type, lhs: Term, rhs: Term): Term =
    t match {
      case _: structure.Type.PrimitiveType => q"$lhs == $rhs"
      case _ =>
        q"$lhs.asInstanceOf[_root_.scala.AnyRef] eq $rhs.asInstanceOf[_root_.scala.AnyRef]"
    }
}
