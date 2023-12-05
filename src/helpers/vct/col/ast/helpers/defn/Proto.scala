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

  case class Field(name: String, index: Int, t: Type) {
    def write(out: Appendable): Unit = {
      t.write(out)
      out.append(" ")
      out.append(name)
      out.append(" = ")
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

  def explode(name: String): Seq[String] =
    name.split("_").toIndexedSeq.flatMap(explodeCamel(_))

  @tailrec
  def explodeCamel(name: String, acc: Seq[String] = Nil): Seq[String] = {
    if (name.isEmpty) return acc

    val (leftUpper, rem0) = name.span(_.isUpper)

    if (leftUpper.length <= 1) {
      // FunctionInvocation -> Function +: explodeCamel("Invocation")
      val (leftLower, rem1) = rem0.span(!_.isUpper)
      explodeCamel(rem1, acc :+ (leftUpper + leftLower).toLowerCase)
    } else {
      // ADTFunctionInvocation -> ADT +: explodeCamel("FunctionInvocation")
      explodeCamel(s"${leftUpper.last}${rem0}", acc :+ leftUpper.init.toLowerCase)
    }
  }

  private val bannedParts = Set(
    Seq("instance", "of"),
    Seq("class"),
    Seq("empty"),
    Seq("assert"),
  )

  def parts(name: String): Seq[String] = {
    val parts = explode(name)
    if(bannedParts.contains(parts))
      "vct" +: parts
    else parts
  }

  def snake(name: String): String =
    parts(name).mkString("_")

  def ucamel(name: String): String =
    parts(name).map(_.capitalize).mkString("")

  def getType(t: structure.Name): MessageType =
    MessageType(t.parts.init.map(snake) :+ ucamel(t.parts.last))

  case class PrimitiveTypeResult(t: PrimitiveType, auxs: Seq[Message] = Nil, imports: Seq[Seq[String]] = Nil) {
    def result(trafo: PrimitiveType => Type): TypeResult =
      TypeResult(trafo(t), auxs, imports)
  }

  case class TypeResult(t: Type, auxs: Seq[Message] = Nil, imports: Seq[Seq[String]] = Nil)

  def typeText(t: structure.Type): String = t match {
    case Type.Node(name) => name.tailName.parts.flatMap(parts).map(_.capitalize).mkString("")
    case Type.Declaration(name) => typeText(Type.Node(name))
    case Type.DeclarationSeq(name) => typeText(Type.Seq(Type.Node(name)))
    case Type.Ref(node) => s"Ref_${typeText(node)}"
    case Type.MultiRef(node) => s"Ref_${typeText(node)}"
    case Type.Tuple(args) => s"Tuple${args.size}_${args.map(typeText).mkString("_")}"
    case Type.Option(arg) => s"Option_${typeText(arg)}"
    case Type.Seq(arg) => s"Seq_${typeText(arg)}"
    case Type.Either(left, right) => s"Either_${typeText(left)}_${typeText(right)}"
    case other: Type.PrimitiveType => other.toString
  }

  private val _getTupleAux = mutable.Map[Seq[structure.Type], PrimitiveTypeResult]()
  def getTupleAux(ts: Seq[structure.Type]): PrimitiveTypeResult =
    _getTupleAux.getOrElseUpdate(ts, {
      val fieldTypeResults = ts.map(getType)
      val auxs = fieldTypeResults.flatMap(_.auxs)
      val imports = fieldTypeResults.flatMap(_.imports)
      val fieldTypes = fieldTypeResults.map(_.t)
      val fields = MessageFields(fieldTypes.zipWithIndex.map {
        case (t, i) => Field(s"v${i+1}", i+1, t)
      })
      val message = Message(auxBase :+ typeText(structure.Type.Tuple(ts)), fields)
      PrimitiveTypeResult(MessageType(message.name), auxs :+ message, imports)
    })

  private val _getOptionAux = mutable.Map[structure.Type, PrimitiveTypeResult]()
  def getOptionAux(t: structure.Type): PrimitiveTypeResult =
    _getOptionAux.getOrElseUpdate(t, {
      val typeResult = getPrimitiveType(t)
      val field = Field("value", 1, Option(typeResult.t))
      val message = Message(auxBase :+ ("Option_" + typeText(t)), MessageFields(Seq(field)))
      PrimitiveTypeResult(MessageType(message.name), typeResult.auxs :+ message, typeResult.imports)
    })

  private val _getSeqAux = mutable.Map[structure.Type, PrimitiveTypeResult]()
  def getSeqAux(t: structure.Type): PrimitiveTypeResult =
    _getSeqAux.getOrElseUpdate(t, {
      val typeResult = getPrimitiveType(t)
      val field = Field("value", 1, Option(typeResult.t))
      val message = Message(auxBase :+ ("Seq_" + typeText(t)), MessageFields(Seq(field)))
      PrimitiveTypeResult(MessageType(message.name), typeResult.auxs :+ message, typeResult.imports)
    })

  private val _getEitherAux = mutable.Map[(structure.Type, structure.Type), PrimitiveTypeResult]()
  def getEitherAux(left: structure.Type, right: structure.Type): PrimitiveTypeResult =
    _getEitherAux.getOrElseUpdate((left, right), {
      val leftTypeResult = getPrimitiveType(left)
      val rightTypeResult = getPrimitiveType(right)
      val leftField = Field("left", 1, Required(leftTypeResult.t))
      val rightField = Field("right", 2, Required(rightTypeResult.t))
      val fields = MessageOneOf("either", Seq(leftField, rightField))
      val message = Message(auxBase :+ s"Either_${typeText(left)}_${typeText(right)}", fields)
      PrimitiveTypeResult(
        MessageType(message.name),
        (leftTypeResult.auxs ++ rightTypeResult.auxs) :+ message,
        leftTypeResult.imports ++ rightTypeResult.imports
      )
    })

  def getStandardType(name: String): PrimitiveTypeResult =
    PrimitiveTypeResult(MessageType(auxBase :+ name), imports=Seq(auxBase :+ name))

  private val _getPrimitiveType = mutable.Map[structure.Type, PrimitiveTypeResult]()
  def getPrimitiveType(t: structure.Type): PrimitiveTypeResult =
    _getPrimitiveType.getOrElseUpdate(t, {
      t match {
        case Type.Node(name) =>
          PrimitiveTypeResult(getType(name.tailName), imports=Seq(name.tailName.parts))
        case Type.Declaration(name) =>
          PrimitiveTypeResult(getType(name.tailName), imports=Seq(name.tailName.parts))
        case Type.Ref(_) | Type.MultiRef(_) => getStandardType("Ref")
        case Type.Tuple(args) => getTupleAux(args)

        case Type.Nothing => PrimitiveTypeResult(Bool)
        case Type.Unit => PrimitiveTypeResult(Bool)
        case Type.String => PrimitiveTypeResult(String)
        case Type.BigInt => getStandardType("BigInt")
        case Type.BigDecimal => getStandardType("BigDecimal")
        case Type.BitString => getStandardType("BitString")
        case Type.ExpectedError => getStandardType("ExpectedError")
        case Type.Boolean => PrimitiveTypeResult(Bool)
        case Type.Byte => PrimitiveTypeResult(Int)
        case Type.Short => PrimitiveTypeResult(Int)
        case Type.Int => PrimitiveTypeResult(Int)
        case Type.Long => PrimitiveTypeResult(Long)
        case Type.Float => PrimitiveTypeResult(Float)
        case Type.Double => PrimitiveTypeResult(Double)
        case Type.Char => PrimitiveTypeResult(Int)

        case Type.Seq(t) => getSeqAux(t)
        case Type.DeclarationSeq(name) => getSeqAux(Type.Declaration(name))
        case Type.Option(t) => getOptionAux(t)
        case Type.Either(left, right) => getEitherAux(left, right)
      }
    })

  private val _getType = mutable.Map[structure.Type, TypeResult]()
  def getType(t: structure.Type): TypeResult =
    _getType.getOrElseUpdate(t, t match {
      case Type.Seq(t) => getPrimitiveType(t).result(Repeated)
      case Type.Option(t) => getPrimitiveType(t).result(Option)
      case _ => getPrimitiveType(t).result(Required)
    })
}
