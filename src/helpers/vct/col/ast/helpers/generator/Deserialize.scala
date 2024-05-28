package vct.col.ast.helpers.generator

import vct.col.ast.helpers.defn.Constants._
import vct.col.ast.helpers.defn.Naming._
import vct.col.ast.helpers.defn.{Proto, ProtoNaming}
import vct.col.ast.structure.{
  DeclaredNode,
  NodeDefinition,
  NodeGenerator,
  NodeKind,
  Type => ST,
}

import java.nio.file.Path
import scala.meta._

class Deserialize extends NodeGenerator {
  override def generate(out: Path, node: NodeDefinition): Unit =
    ResultStream.write(
      out.resolve("Deserialize" + node.name.base + ".scala"),
      deserialize(node),
    )

  def deserialize(node: NodeDefinition): Source =
    source"""
      package $DeserializePackage

      object ${deserializeObjectName(node)} {
        def deserialize[G](node: ${scalapbType(node.name)}, decls: $MutMap[$Long, $Declaration[G]]): ${typ(node)}[G] =
          ${deserializeNode(q"node", node)}
      }
    """

  def deserializeNode(term: Term, node: NodeDefinition): Term = {
    val fields =
      node.fields.map { case (name, structuralType) =>
        val scalapbName = ProtoNaming.camel(ProtoNaming.snake(name))
        val fieldTerm = q"$term.${Term.Name(scalapbName)}"
        val protoType = ProtoNaming.getType(structuralType).t
        deserializeTerm(fieldTerm, protoType, structuralType)
      }.toList

    val instance =
      if (node.blameType.isEmpty)
        q"new ${t"${typ(node)}[G]"}(..$fields)($SerializeOrigin.deserialize($term.origin))"
      else
        q"""
          {
            val `~o`: $Origin = $SerializeOrigin.deserialize($term.origin)
            new ${t"${typ(node)}[G]"}(..$fields)($SerializeBlame.deserialize($term.blame, `~o`))(`~o`)
          }
        """

    if (node.kind == DeclaredNode)
      q"{ val res = $instance; decls($term.id) = res; res }"
    else
      instance
  }

  def err(pt: Any, st: ST): Nothing = {
    System.err.println("Unknown pair of protobuf type and structural type:")
    System.err.println(s" - protobuf: $pt")
    System.err.println(s" - structural: $st")
    throw new IllegalArgumentException()
  }

  def deserializeTerm(
      term: Term,
      protoType: Proto.Type,
      structuralType: ST,
  ): Term =
    (protoType, structuralType) match {
      case (Proto.Required(pt), st) => deserializeTerm(term, pt, st)
      case (Proto.Option(pt), ST.Option(st)) =>
        q"$term.map(`~x` => ${deserializeTerm(q"`~x`", pt, st)})"
      case (Proto.Repeated(pt), ST.Seq(st)) =>
        q"$term.map(`~x` => ${deserializeTerm(q"`~x`", pt, st)})"
      case (Proto.Repeated(_), ST.DeclarationSeq(name)) =>
        q"$term.map(`~x` => ${deserializeFamilyObject(name)}.deserialize[G](`~x`.parseAs[${scalapbType(name)}], decls))"
      case (pt, st) => err(pt, st)
    }

  def deserializeTerm(
      term: Term,
      protoType: Proto.PrimitiveType,
      structuralType: ST,
  ): Term =
    (protoType, structuralType) match {
      case (_, ST.Seq(ST.ExpectedError)) => q"$SeqObj.empty"

      case (Proto.FamilyType(_), ST.Node(name)) =>
        q"${deserializeFamilyObject(name)}.deserialize[G]($term.parseAs[${scalapbType(name)}], decls)"
      case (Proto.FamilyType(_), ST.Declaration(name)) =>
        q"${deserializeFamilyObject(name)}.deserialize[G]($term.parseAs[${scalapbType(name)}], decls)"
      case (Proto.StandardType(_), ST.Ref(node)) =>
        q"new ${Init(t"$LazyRef[G, ${typ(node.name)}[G]]", Name.Anonymous(), List(List(q"decls($term.id)")))}"
      case (Proto.StandardType(_), ST.MultiRef(node)) =>
        q"new ${Init(t"$LazyRef[G, ${typ(node.name)}[G]]", Name.Anonymous(), List(List(q"decls($term.id)")))}"
      case (Proto.AuxType(_), ST.Tuple(args)) =>
        q"(..${args.zipWithIndex.map { case (structuralType, index) =>
            deserializeTerm(q"$term.${Term.Name(s"v${index + 1}")}", ProtoNaming.getType(structuralType).t, structuralType)
          }.toList})"
      case (Proto.AuxType(_), ST.Seq(structuralType)) =>
        q"$term.value.map(`~x` => ${deserializeTerm(q"`~x`", ProtoNaming.getType(structuralType).t, structuralType)})"
      case (Proto.AuxType(_), ST.DeclarationSeq(name)) =>
        q"$term.value.map(`~x` => ${deserializeFamilyObject(name)}.deserialize[G](`~x`.parseAs[${scalapbType(name)}], decls))"
      case (Proto.AuxType(_), ST.Option(structuralType)) =>
        q"$term.value.map(`~x` => ${deserializeTerm(q"`~x`", ProtoNaming.getPrimitiveType(structuralType).t, structuralType)})"
      case (Proto.AuxType(_), ST.Either(left, right)) =>
        q"""
          if($term.either.isLeft) ${deserializeTerm(q"$term.left.get.value", ProtoNaming.getPrimitiveType(left).t, left)}
          else ${deserializeTerm(q"$term.right.get.value", ProtoNaming.getPrimitiveType(right).t, right)}
        """
      case (Proto.Bool, ST.Nothing) => q"???"
      case (Proto.Bool, ST.Unit) => q"()"
      case (Proto.String, ST.String) => term
      case (Proto.StandardType(_), ST.BigInt) =>
        q"new $BigInt(new $BigInteger($term.data.toByteArray))"
      case (Proto.StandardType(_), ST.BigDecimal) =>
        q"new $BigDecimal(new $JBigDecimal(new $BigInteger($term.unscaledValue.data.toByteArray), $term.scale))"
      case (Proto.StandardType(_), ST.BitString) =>
        q"new $BitString($term.data.toByteArray, $term.skipAtLastByte)"
      case (Proto.StandardType(_), ST.ExpectedError) => q"???"
      case (Proto.Bool, ST.Boolean) => term
      case (Proto.Int, ST.Byte) => q"$term.toByte"
      case (Proto.Int, ST.Short) => q"$term.toShort"
      case (Proto.Int, ST.Int) => term
      case (Proto.Long, ST.Long) => term
      case (Proto.Float, ST.Float) => term
      case (Proto.Double, ST.Double) => term
      case (Proto.Int, ST.Char) => q"$term.toChar"
      case (pt, st) => err(pt, st)
    }

}
