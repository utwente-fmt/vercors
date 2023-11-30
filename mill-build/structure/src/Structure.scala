package vct.col.ast.structure

import upickle.default.{macroRW, ReadWriter => RW}

/**
 * Note: as a matter of style I'm keeping the sources in mill-build as small as possible. That is: we only program
 * in the meta-build what is required to compute the shape of the real build. For us this includes some bits of the
 * structure of the node definitions, so we can instantiate tasks based on all the nodes, all the families, etc. We
 * nevertheless extract a bit more information than strictly necessary, just so we don't have to read the node files
 * again.
 */

/**
 * Sequence of name parts. For node types they should be fully qualified.
 */
case class Name(parts: Seq[String]) {
  def +(other: String): Name = Name(parts :+ other)
  def +(other: Name): Name = Name(parts ++ other.parts)
  def +:(other: String): Name = Name(other +: parts)

  def base: String = parts.last
  def baseName: Name = Name(Seq(base))

  def initName: Name = Name(parts.init)
  def tailName: Name = Name(parts.tail)
}

object Name {
  implicit val rw: RW[Name] = macroRW
  implicit val segments: mill.define.Cross.ToSegments[Name] = new mill.define.Cross.ToSegments(n => n.parts.toList)
}

/**
 * Virtual types that constrain all types slightly to what is allowed in nodes.
 */
sealed trait Type

object Type {
  implicit val rw: RW[Type] = RW.merge(Node.rw, Ref.rw, MultiRef.rw, Tuple.rw, Seq.rw, Option.rw, Either.rw, PrimitiveType.rw)

  /**
   * A type that supports the root node type, and takes exactly one type parameter: the generation. This is not
   * necessarily a concrete node: it may be an intermediate category, a family, or Node itself.
   */
  case class Node(name: Name) extends Type

  object Node {
    implicit val rw: RW[Node] = macroRW
  }

  /**
   * Exactly the Ref type, which takes a generation parameter and a node type parameter.
   */
  case class Ref(node: Node) extends Type

  object Ref {
    implicit val rw: RW[Ref] = macroRW
  }

  case class MultiRef(node: Node) extends Type

  object MultiRef {
    implicit val rw: RW[MultiRef] = macroRW
  }

  case class Tuple(args: scala.Seq[Type]) extends Type

  object Tuple {
    implicit val rw: RW[Tuple] = macroRW
  }

  case class Seq(arg: Type) extends Type

  object Seq {
    implicit val rw: RW[Seq] = macroRW
  }

  case class Option(arg: Type) extends Type

  object Option {
    implicit val rw: RW[Option] = macroRW
  }

  case class Either(left: Type, right: Type) extends Type

  object Either {
    implicit val rw: RW[Either] = macroRW
  }

  sealed trait PrimitiveType extends Type

  object PrimitiveType {
    implicit val rw: RW[PrimitiveType] =
      RW.merge(ValueType.rw, macroRW[Nothing.type], macroRW[Unit.type], macroRW[String.type], macroRW[BigInt.type], macroRW[BigDecimal.type], macroRW[BitString.type], macroRW[ExpectedError.type])
  }

  object Nothing extends PrimitiveType
  object Unit extends PrimitiveType
  object String extends PrimitiveType
  object BigInt extends PrimitiveType
  object BigDecimal extends PrimitiveType
  object BitString extends PrimitiveType
  object ExpectedError extends PrimitiveType

  sealed trait ValueType extends PrimitiveType

  object ValueType {
    implicit val rw: RW[ValueType] =
      RW.merge(macroRW[Boolean.type], macroRW[Byte.type], macroRW[Short.type], macroRW[Int.type], macroRW[Long.type], macroRW[Float.type], macroRW[Double.type], macroRW[Char.type])
  }

  object Boolean extends ValueType
  object Byte extends ValueType
  object Short extends ValueType
  object Int extends ValueType
  object Long extends ValueType
  object Float extends ValueType
  object Double extends ValueType
  object Char extends ValueType
}

sealed trait NodeKind

object NodeKind {
  implicit val rw: RW[NodeKind] = RW.merge(macroRW[StructuralNode.type], macroRW[DeclaredNode.type])
}

object StructuralNode extends NodeKind
object DeclaredNode extends NodeKind

/**
 * All the parts that define a concrete node. Concrete nodes can be classes or case classes.
 * @param name The fully qualified name of the node type
 * @param fields The fields that make up the node, not including the blame or origin
 * @param blameType Empty if there is no blame, otherwise the type argument to the blame parameter
 * @param supports extends/with types of this node.
 */
case class NodeDefinition(name: Name, fields: Seq[(String, Type)], blameType: Option[Name], kind: NodeKind)

object NodeDefinition {
  implicit val rw: RW[NodeDefinition] = macroRW
  implicit val segments: mill.define.Cross.ToSegments[NodeDefinition] = new mill.define.Cross.ToSegments(n => n.name.parts.toList)
}