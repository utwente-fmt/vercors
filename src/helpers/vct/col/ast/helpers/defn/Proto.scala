package vct.col.ast.helpers.defn

import vct.col.ast.structure
import vct.col.ast.structure.Type

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

object Proto {
  val auxBase = Seq("vct", "col", "ast")

  val STANDARD_OPTIONS: Map[String, String] = ListMap(
    "lenses" -> "false",
    "getters" -> "false",
    "no_default_values_in_constructor" -> "true",
  )

  val OPAQUE_SUBMESSAGES_OPTIONS: Map[String, String] = STANDARD_OPTIONS ++ ListMap(
    "bytes_type" -> "\"_root_.vct.serialize.OpaqueMessageBytes\"",
  )

  def renderOptions(options: Map[String, String]): String =
    s"""option (scalapb.options) = {
       |  ${options.map { case (k, v) => s"$k: $v" }.mkString("\n  ")}
       |};""".stripMargin

  sealed trait PrimitiveType {
    def write(out: Appendable): Unit = this match {
      case Int => out.append("sint32")
      case Float => out.append("float")
      case Long => out.append("sint64")
      case Double => out.append("double")
      case Bool => out.append("bool")
      case String => out.append("string")
      case Bytes => out.append("bytes")
      case t: NamedType =>
        for(part <- t.fqName) {
          out.append('.')
          out.append(part)
        }
    }

    def opaqueNodes: PrimitiveType = this match {
      case FamilyType(_) => Bytes
      case other => other
    }
  }

  case object Int extends PrimitiveType
  case object Float extends PrimitiveType
  case object Long extends PrimitiveType
  case object Double extends PrimitiveType
  case object Bool extends PrimitiveType
  case object String extends PrimitiveType
  case object Bytes extends PrimitiveType
  sealed trait NamedType extends PrimitiveType { def fqName: Seq[String] }
  case class StandardType(fqName: Seq[String]) extends NamedType
  case class AuxType(fqName: Seq[String]) extends NamedType
  case class FamilyType(fqName: Seq[String]) extends NamedType

  object Type {
    def unapply(t: Type): Some[PrimitiveType] = Some(t.t)
  }

  sealed trait Type {
    def t: PrimitiveType

    def write(out: Appendable): Unit = this match {
      case Option(t) => out.append("optional "); t.write(out)
      case Repeated(t) => out.append("repeated "); t.write(out)
      case Required(t) => out.append("required "); t.write(out)
      case UnspecifiedArity(t) => t.write(out)
    }

    def writeOption(out: Appendable): Unit = this match {
      case Repeated(Int) | Repeated(Long) | Repeated(Bool) =>
        out.append(" [packed = true]")
      case _ => // Do nothing
    }

    def map(f: PrimitiveType => PrimitiveType): Type = this match {
      case Option(t) => Option(f(t))
      case Repeated(t) => Repeated(f(t))
      case Required(t) => Required(f(t))
      case UnspecifiedArity(t) => UnspecifiedArity(f(t))
    }
  }

  case class Option(t: PrimitiveType) extends Type
  case class Repeated(t: PrimitiveType) extends Type
  case class Required(t: PrimitiveType) extends Type
  case class UnspecifiedArity(t: PrimitiveType) extends Type

  case class Field(name: String, index: Int, t: Type) {
    def write(out: Appendable): Unit = {
      t.write(out)
      out.append(" ")
      out.append(name)
      out.append(" = ")
      out.append(index.toString)
      t.writeOption(out)
      out.append(";")
    }

    def opaqueNodes: Field =
      copy(t = t.map(_.opaqueNodes))
  }

  sealed trait MessageBody {
    def fields: Seq[Field]

    def namedTypes: Seq[NamedType] =
      fields.collect {
        case Field(_, _, Type(name: NamedType)) => name
      }

    private def writeFields(out: Appendable, indent: String): Unit =
      for(field <- fields) {
        out.append(indent)
        field.write(out)
        out.append("\n")
      }

    def write(out: Appendable): Unit = this match {
      case MessageFields(_) =>
        writeFields(out, "  ")
      case MessageOneOf(oneOfName, _) =>
        out.append("  oneof ").append(oneOfName).append(" {\n")
        writeFields(out, "    ")
        out.append("  }\n")
    }

    def opaqueNodes: MessageBody
  }

  case class MessageFields(fields: Seq[Field]) extends MessageBody {
    def opaqueNodes: MessageFields = copy(fields = fields.map(_.opaqueNodes))
  }

  case class MessageOneOf(oneOfName: String, fields: Seq[Field]) extends MessageBody {
    def opaqueNodes: MessageOneOf = copy(fields = fields.map(_.opaqueNodes))
  }

  case class Message(name: Seq[String], body: MessageBody) {
    require(name.nonEmpty)

    def write(out: Appendable): Unit = {
      out.append("message ").append(name.last).append(" {\n")
      body.write(out)
      out.append("}\n")
    }

    def opaqueNodes: Message =
      copy(body = body.opaqueNodes)

    def namedTypes: Seq[NamedType] = body.namedTypes

    def imports: Seq[Seq[String]] = namedTypes.distinct.map(_.fqName)
  }

  object Source {
    def apply(imports: Seq[Seq[String]], options: String, message: Message): Source =
      Source(imports, options, Seq(message))
  }

  case class Source(imports: Seq[Seq[String]], options: String, messages: Seq[Message]) {
    require(messages.nonEmpty)

    def write(out: Appendable): Unit = {
      out.append("syntax = \"proto2\";\n")
      out.append("\n")

      if(messages.exists(_.name.size > 1)) {
        val pkg = messages.collectFirst { case msg if msg.name.size > 1 => msg.name.init }.get

        val wrongPackageMessages = messages.filter(msg => msg.name.size != pkg.size + 1 || msg.name.init != pkg)
        require(wrongPackageMessages.isEmpty,
          out.append(s"Messages rendered jointly in one source must have the same package. Wrong: ${
            wrongPackageMessages.map(_.name.mkString(".")).mkString("{", ", ", "}")}"))

        out.append("package ").append(pkg.mkString(".")).append(";\n")
        out.append("\n")
      }

      if(imports.nonEmpty) {
        for(imp <- imports) {
          out.append("import \"").append(imp.mkString("/")).append(".proto\";\n")
        }
        out.append("\n")
      }

      if(!options.isBlank) {
        out.append(options)
        out.append("\n")
      }

      for(message <- messages) {
        out.append("\n")
        message.write(out)
      }
    }

    def opaqueNodes: Source =
      copy(messages = messages.map(_.opaqueNodes))
  }
}
