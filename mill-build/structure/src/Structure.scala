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
  implicit val rw: RW[Type] = RW.merge(Node.rw, Ref.rw, Generation.rw, Tuple.rw, Other.rw)

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

  object Generation extends Type {
    implicit val rw: RW[Generation.type] = macroRW
  }

  /**
   * A tuple type, since it is a special case in scalameta.
   */
  case class Tuple(args: Seq[Type]) extends Type

  object Tuple {
    implicit val rw: RW[Tuple] = macroRW
  }

  /**
   * Other types, such as collections (Seq, Set) and primitive types (BigInt, String)
   */
  case class Other(name: Name, args: Seq[Type]) extends Type

  object Other {
    implicit val rw: RW[Other] = macroRW
  }
}

sealed trait AnyNodeDeclaration {
  def name: Name
  def supports: Seq[Type]
}

object AnyNodeDeclaration {
  implicit val rw: RW[AnyNodeDeclaration] = RW.merge(NodeDefinition.rw, NodeCategory.rw)
}

/**
 * All the parts that define a concrete node. Concrete nodes can be classes or case classes.
 * @param name The fully qualified name of the node type
 * @param fields The fields that make up the node, not including the blame or origin
 * @param blameType Empty if there is no blame, otherwise the type argument to the blame parameter
 * @param supports extends/with types of this node.
 */
case class NodeDefinition(name: Name, fields: Seq[(String, Type)], blameType: Option[Name], supports: Seq[Type])
    extends AnyNodeDeclaration

object NodeDefinition {
  implicit val rw: RW[NodeDefinition] = macroRW
  implicit val segments: mill.define.Cross.ToSegments[NodeDefinition] = new mill.define.Cross.ToSegments(n => n.name.parts.toList)
}

/**
 * Any intermediate category of nodes, or the root Node type. Categories can be abstract classes or traits.
 * @param name The fully qualified name of the category
 * @param supports extends/with types of this node.
 */
case class NodeCategory(name: Name, supports: Seq[Type])
    extends AnyNodeDeclaration

object NodeCategory {
  implicit val rw: RW[NodeCategory] = macroRW
}