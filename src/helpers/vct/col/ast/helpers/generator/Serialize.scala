package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming._
import vct.col.ast.helpers.defn.{Naming, Proto, ProtoNaming}
import vct.col.ast.structure
import vct.col.ast.structure.{NodeDefinition, NodeGenerator, Type => ST}

import java.nio.file.Path
import scala.meta._

class Serialize extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit =
    ResultStream.write(out.resolve(s"${node.name.base}Serialize.scala"), serialize(node))

  def serialize(node: NodeDefinition): Source =
    source"""
      package $SerializePackage

      trait ${serializeTrait(node)}[G] { this: ${typ(node)}[G] =>
        def serialize(): ${scalapbType(node.name)} =
          new ${scalapbType(node.name)}(..${node.fields.map { case (name, t) => serializeField(name, t) }.toList})

        def serializeFamily(): ${scalapbType(node.family)} =
          ${serializeFamily(node)}
      }
    """

  def serializeFamily(node: NodeDefinition): Term =
    if (node.name == node.family)
      q"this.serialize()"
    else
      q"""
        new ${scalapbType(node.family)}(
          new ${Type.Select(q"${Naming.term(scalapbName(node.family))}.V", Type.Name(ProtoNaming.ucamel(node.name.base)))}(
            this.serialize()
          )
        )
      """

  def err(st: ST, pt: Any): Nothing = {
    System.err.println("Unknown pair of structural type and protobuf type:")
    System.err.println(s" - structural: $st")
    System.err.println(s" - protobuf: $pt")
    throw new IllegalArgumentException()
  }

  def serializeField(name: String, structureType: structure.Type): Term = {
    val protoType = ProtoNaming.getType(structureType).t
    val value = q"this.${Term.Name(name)}"
    serializeTerm(value, structureType, protoType)
  }

  def serializeTerm(term: Term, structureType: structure.Type, protoType: Proto.Type): Term =
    (structureType, protoType) match {
      case (st, Proto.Required(pt)) =>
        serializeTerm(term, st, pt)
      case (ST.Option(st), Proto.Option(pt)) =>
        q"$term.map(`~x` => ${serializeTerm(q"`~x`", st, pt)})"
      case (ST.Seq(st), Proto.Repeated(pt)) =>
        q"$term.map(`~x` => ${serializeTerm(q"`~x`", st, pt)})"
      case (ST.DeclarationSeq(name), Proto.Repeated(pt)) =>
        q"$term.map(`~x` => new $NodeMessageView(`~x`))"
      case (st, pt) =>
        err(st, pt)
    }

  def mk(name: Seq[String], args: Term*): Term =
    q"new ${typ(structure.Name(name))}(..${args.toList})"

  def serializeTerm(term: Term, structureType: ST, protoType: Proto.PrimitiveType): Term =
    (structureType, protoType) match {
      case (ST.Node(_), Proto.FamilyType(_)) =>
        q"new $NodeMessageView($term)"
      case (ST.Declaration(_), Proto.FamilyType(_)) =>
        q"new $NodeMessageView($term)"
      case (ST.DeclarationSeq(_), Proto.AuxType(name)) =>
        mk(name, q"$term.map(`~x` => new $NodeMessageView(`~x`))")
      case (ST.Ref(_), Proto.StandardType(name)) =>
        mk(name, q"0L")
      case (ST.MultiRef(_), Proto.StandardType(name)) =>
        mk(name, q"0L")
      case (ST.Tuple(args), Proto.AuxType(name)) =>
        mk(name, args.zipWithIndex.map { case (structureType, idx) =>
          val protoType = ProtoNaming.getType(structureType).t
          val field = Term.Name(s"_${idx+1}")
          serializeTerm(q"$term.$field", structureType, protoType)
        }: _*)
      case (ST.Seq(structureType), Proto.AuxType(name)) =>
        val protoType = ProtoNaming.getType(structureType).t
        mk(name, q"$term.map(`~x` => ${serializeTerm(q"`~x`", structureType, protoType)})")
      case (ST.Option(structureType), Proto.AuxType(name)) =>
        val protoType = ProtoNaming.getType(structureType).t
        mk(name, q"$term.map(`~x` => ${serializeTerm(q"`~x`", structureType, protoType)})")
      case (ST.Either(left, right), Proto.AuxType(name)) =>
        mk(name, q"""
          $term.fold(
            `~x` => ${mk(name :+ "V" :+ "Left", serializeTerm(q"`~x`", left, ProtoNaming.getPrimitiveType(left).t))},
            `~x` => ${mk(name :+ "V" :+ "Right", serializeTerm(q"`~x`", right, ProtoNaming.getPrimitiveType(right).t))},
          )
        """)
      case (ST.Nothing, Proto.Bool) =>
        term
      case (ST.Unit, Proto.Bool) =>
        q"true"
      case (ST.String, Proto.String) =>
        term
      case (ST.BigInt, Proto.StandardType(name)) =>
        mk(name, q"$copyByteStringFrom($term.toByteArray)")
      case (ST.BigDecimal, Proto.StandardType(name)) =>
        mk(name, q"$term.scale", serializeTerm(q"$term.underlying.unscaledValue", ST.BigInt, ProtoNaming.getStandardType("BigInt").t))
      case (ST.BitString, Proto.StandardType(name)) =>
        mk(name, q"$copyByteStringFrom($term.rawData)", q"$term.skipAtLastByte")
      case (ST.ExpectedError, Proto.StandardType(name)) =>
        mk(name)
      case (ST.Boolean, Proto.Bool) =>
        term
      case (ST.Byte, Proto.Int) =>
        term
      case (ST.Short, Proto.Int) =>
        term
      case (ST.Int, Proto.Int) =>
        term
      case (ST.Long, Proto.Long) =>
        term
      case (ST.Float, Proto.Float) =>
        term
      case (ST.Double, Proto.Double) =>
        term
      case (ST.Char, Proto.Int) =>
        term
      case (st, pt) => err(st, pt)
    }
}
