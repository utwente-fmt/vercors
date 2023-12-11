package vct.col.ast.helpers.defn

import vct.col.ast.helpers.defn.Constants.{ComparePackage, DeclarePackage, RefType, RewritePackage, SerializePackage, SubnodesPackage}
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

  def typ(node: structure.NodeDefinition): Type = typ(node.name)

  def typ(t: structure.Type, g: Type): Type = t match {
    case structure.Type.Node(name) => t"${typ(name)}[$g]"
    case structure.Type.Declaration(name) => t"${typ(name)}[$g]"
    case structure.Type.DeclarationSeq(name) => t"_root_.scala.Seq[${typ(name)}[$g]]"
    case structure.Type.Ref(node) => t"$RefType[$g, ${typ(node.name)}[$g]]"
    case structure.Type.MultiRef(node) => t"$RefType[$g, ${typ(node.name)}[$g]]"
    case structure.Type.Tuple(args) => t"(..${args.toList.map(typ(_, g))})"
    case structure.Type.Seq(arg) => t"_root_.scala.Seq[${typ(arg, g)}]"
    case structure.Type.Option(arg) => t"_root_.scala.Option[${typ(arg, g)}]"
    case structure.Type.Either(left, right) => t"_root_.scala.util.Either[${typ(left, g)}, ${typ(right, g)}]"
    case structure.Type.Nothing => t"_root_.scala.Nothing"
    case structure.Type.Unit => t"_root_.scala.Unit"
    case structure.Type.String => t"_root_.java.lang.String"
    case structure.Type.BigInt => t"_root_.scala.BigInt"
    case structure.Type.BigDecimal => t"_root_.scala.BigDecimal"
    case structure.Type.BitString => t"_root_.hre.data.BitString"
    case structure.Type.ExpectedError => t"_root_.vct.col.origin.ExpectedError"
    case structure.Type.Boolean => t"_root_.scala.Boolean"
    case structure.Type.Byte => t"_root_.scala.Byte"
    case structure.Type.Short => t"_root_.scala.Short"
    case structure.Type.Int => t"_root_.scala.Int"
    case structure.Type.Long => t"_root_.scala.Long"
    case structure.Type.Float => t"_root_.scala.Float"
    case structure.Type.Double => t"_root_.scala.Double"
    case structure.Type.Char => t"_root_.scala.Char"
  }

  def packageFromRoot(t: Term): Term.Ref = t match {
    case name: Term.Name => q"_root_.$name"
    case Term.Select(base, name) => q"${packageFromRoot(base)}.$name"
    case _ => ???
  }

  def scopes(family: String): Term.Name =
    Term.Name(family.charAt(0).toLower.toString + family.substring(1) + "s")

  def scalapbName(node: structure.Name): structure.Name = {
    val name = ProtoNaming.getTypeName(node)
    val pkg = "_root_" +: ProtoNaming.scalaPackage(name)
    val baseName = name.last
    structure.Name(pkg :+ baseName)
  }

  def scalapbType(node: structure.Name): Type =
    typ(scalapbName(node))

  def opsTrait(node: NodeDefinition) = Type.Name(node.name.base + "Ops")
  def opsFamilyTrait(node: structure.Name) = Type.Name(node.base + "FamilyOps")

  def compareTrait(node: NodeDefinition) = Type.Name(node.name.base + "Compare")
  def compareType(node: NodeDefinition) = t"${packageFromRoot(ComparePackage)}.${compareTrait(node)}"
  def rewriteTrait(node: NodeDefinition) = Type.Name(node.name.base + "Rewrite")
  def rewriteType(node: NodeDefinition) = t"${packageFromRoot(RewritePackage)}.${rewriteTrait(node)}"
  def subnodesTrait(node: NodeDefinition) = Type.Name(node.name.base + "Subnodes")
  def subnodesType(node: NodeDefinition) = t"${packageFromRoot(SubnodesPackage)}.${subnodesTrait(node)}"
  def serializeTrait(node: NodeDefinition) = Type.Name(node.name.base + "Serialize")
  def serializeType(node: NodeDefinition) = t"${packageFromRoot(SerializePackage)}.${serializeTrait(node)}"
  def declareTrait(node: structure.Name) = Type.Name(node.base + "FamilyDeclare")
  def declareType(node: structure.Name) = t"${packageFromRoot(DeclarePackage)}.${declareTrait(node)}"
}
