package vct.col.ast.helpers.defn

import vct.col.ast.structure
import vct.col.ast.structure.Type

import scala.annotation.tailrec
import scala.collection.mutable

object Proto {
  val auxBase = Seq("vct", "col", "ast", "serialize")

  sealed trait PrimitiveType {
    def write(out: Appendable): Unit = this match {
      case Int => out.append("sint32")
      case Float => out.append("float")
      case Long => out.append("sint64")
      case Double => out.append("double")
      case Bool => out.append("bool")
      case String => out.append("string")
      case Bytes => out.append("bytes")
      case MessageType(fqName) =>
        for(part <- fqName) {
          out.append('.')
          out.append(part)
        }
    }
  }

  case object Int extends PrimitiveType
  case object Float extends PrimitiveType
  case object Long extends PrimitiveType
  case object Double extends PrimitiveType
  case object Bool extends PrimitiveType
  case object String extends PrimitiveType
  case object Bytes extends PrimitiveType
  case class MessageType(fqName: Seq[String]) extends PrimitiveType

  sealed trait Type {
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
  }

  sealed trait MessageBody {
    def fields: Seq[Field]

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
  }
  case class MessageFields(fields: Seq[Field]) extends MessageBody
  case class MessageOneOf(oneOfName: String, fields: Seq[Field]) extends MessageBody

  case class Message(name: Seq[String], body: MessageBody) {
    def write(out: Appendable): Unit = {
      out.append("message ").append(name.last).append(" {\n")
      body.write(out)
      out.append("}\n")
    }
  }

  case class Source(imports: Seq[Seq[String]], message: Message) {
    def write(out: Appendable): Unit = {
      out.append("syntax = \"proto2\";\n")
      out.append("\n")

      if(message.name.size > 1) {
        out.append("package ").append(message.name.init.mkString(".")).append(";\n")
        out.append("\n")
      }

      if(imports.nonEmpty) {
        for(imp <- imports) {
          out.append("import \"").append(imp.mkString("/")).append(".proto\";\n")
        }
        out.append("\n")
      }

      message.write(out)
    }
  }
}
