import ColDefs.DECLARATION_KINDS
import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{Type => PType}
import com.google.protobuf.Descriptors.{Descriptor, FieldDescriptor}

import java.io.File
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.meta.{Type => SType}

case class ColProto(info: ColDescription, output: File, writer: (File, String) => Unit) {
  case class Name(orig: String) {
    private val upperIndices = orig.capitalize.zipWithIndex.collect { case (c, i) if c.isUpper => i } :+ orig.length
    private val unsafeParts = upperIndices.zip(upperIndices.tail).map {
      case (from, to) => orig.slice(from, to).toLowerCase
    }

    val parts: Seq[String] = Map(
      Seq("instance", "of") -> Seq("vct", "instance", "of"),
    ).getOrElse(unsafeParts, unsafeParts)

    def snake: String = parts.mkString("_")
    def camel: String = parts.head + parts.tail.map(_.capitalize).mkString("")
    def ucamel: String = parts.map(_.capitalize).mkString("")
  }

  sealed trait Typ {
    def isPrimitive: Boolean = this match {
      case TBool | TRef | TInt | TBigInt | TBigDecimal | TString => true
      case TName(_) => true
      case TOption(t) if t.isMarkable => true
      case TSeq(t) if t.isMarkable => true
      case TSet(t) if t.isMarkable => true
      case _ => false
    }

    def isMarkable: Boolean = this match {
      case TBool | TRef | TInt | TBigInt | TBigDecimal | TString => true
      case TName(_) => true
      case _ => false
    }

    override def toString: String = this match {
      case TBool => "Bool"
      case TRef => "Ref"
      case TInt => "Int"
      case TBigInt => "BigInt"
      case TBigDecimal => "BigDecimal"
      case TString => "String"
      case TName(name) => name
      case TOption(t) => "Opt" + t.toString
      case TSeq(t) => "Seq" + t.toString
      case TSet(t) => "Set" + t.toString
      case TTuple(ts) => ts.mkString("")
    }
  }

  case object TBool extends Typ
  case object TRef extends Typ
  case object TInt extends Typ
  case object TBigInt extends Typ
  case object TBigDecimal extends Typ
  case object TString extends Typ
  case class TName(name: String) extends Typ
  case class TOption(t: Typ) extends Typ
  case class TSeq(t: Typ) extends Typ
  case class TSet(t: Typ) extends Typ
  case class TTuple(ts: Seq[Typ]) extends Typ

  def getType(t: SType): Typ = t match {
    case SType.Apply(SType.Name("Seq"), List(SType.Name("ExpectedError"))) => TName("ExpectedErrors")

    case SType.Apply(SType.Name("Seq"), List(arg)) => TSeq(getType(arg))
    case SType.Apply(SType.Name("Option"), List(arg)) => TOption(getType(arg))
    case SType.Apply(SType.Name("Set"), List(arg)) => TSet(getType(arg))
    case SType.Tuple(ts) => TTuple(ts.map(getType))

    case SType.Apply(SType.Name(decl), List(SType.Name("G"))) if DECLARATION_KINDS.contains(decl) => TName(decl)
    case SType.Apply(SType.Name(node), List(SType.Name("G"))) if info.families.contains(node) => TName(node)

    case SType.Apply(SType.Name("Ref"), List(SType.Name("G"), _)) => TRef
    case SType.Name("Int") => TInt
    case SType.Name("String") => TString
    case SType.Name("Boolean") => TBool
    case SType.Name("BigInt") => TBigInt
    case SType.Name("BigDecimal") => TBigDecimal
  }

  val boxedTypes: mutable.Map[Typ, DescriptorProto] = mutable.Map()

  val boxedTypeForward: mutable.Map[TName, Typ] = mutable.Map()
  val boxedTypeTuple: mutable.Map[TName, TTuple] = mutable.Map()
  val boxedTypeFamily: mutable.Map[TName, String] = mutable.Map()

  def box(t: Typ): TName = {
    TName(boxedTypes.getOrElseUpdate(t, t match {
      case t @ TTuple(ts) =>
        message(t.toString).addAllField(ts.zipWithIndex.map {
          case (t, i) =>
            val f = field(f"v${i+1}")
            setType(f, t)
            f.build()
        }.asJava).build()
      case other =>
        val f = field("v")
        setType(f, t)
        message(other.toString).addField(f).build()
    }).getName)
  }

  def primitivize(t: Typ): Typ = t match {
    case t if t.isPrimitive => t

    case TSeq(inner) if inner.isMarkable => t
    case TSet(inner) if inner.isMarkable => t
    case TOption(inner) if inner.isMarkable => t

    case TSeq(t) => TSeq(box(t))
    case TSet(t) => TSet(box(t))
    case TOption(t) => TOption(box(t))
    case other => box(other)
  }

  def setPrimitivizedType(builder: FieldDescriptorProto.Builder, t: Typ): Unit =
    t match {
      case TBool => builder.setType(PType.TYPE_BOOL)
      case TRef => builder.setTypeName("Ref")
      case TInt => builder.setType(PType.TYPE_INT64)
      case TBigInt => builder.setTypeName("BigInt")
      case TBigDecimal => builder.setTypeName("BigDecimal")
      case TString => builder.setType(PType.TYPE_STRING)
      case TName(name) => builder.setTypeName(name)
      case TOption(t) => builder.setLabel(FieldDescriptorProto.Label.LABEL_OPTIONAL); setType(builder, t)
      case TSeq(t) => builder.setLabel(FieldDescriptorProto.Label.LABEL_REPEATED); setType(builder, t)
      case TSet(t) => builder.setLabel(FieldDescriptorProto.Label.LABEL_REPEATED); setType(builder, t)
      case _ => ???
    }

  def setType(builder: FieldDescriptorProto.Builder, t: Typ): Unit =
    setPrimitivizedType(builder, primitivize(t))

  def setType(builder: FieldDescriptorProto.Builder, t: scala.meta.Type): Unit =
    setType(builder, getType(t))

  def message(name: String): DescriptorProto.Builder =
    DescriptorProto.newBuilder().setName(Name(name).ucamel)

  def field(name: String, t: Option[scala.meta.Type] = None): FieldDescriptorProto.Builder = {
    val res = FieldDescriptorProto.newBuilder().setName(Name(name).snake).setLabel(FieldDescriptorProto.Label.LABEL_REQUIRED)
    t.foreach(setType(res, _))
    res
  }

  def oneOf(name: String): OneofDescriptorProto.Builder =
    OneofDescriptorProto.newBuilder().setName(Name(name).snake)

  def basicTypes(): Seq[DescriptorProto] = Seq(
    message("BigInt")
      .addField(field("data").setType(PType.TYPE_BYTES))
      .build(),
    message("BigDecimal")
      .addField(field("scale").setType(PType.TYPE_INT64))
      .addField(field("unscaledValue").setTypeName("BigInt"))
      .build(),
    message("Ref")
      .addField(field("index").setType(PType.TYPE_INT64))
      .build(),
    message("ExpectedErrors")
      .build(),
  )

  def declarationKinds(): Seq[DescriptorProto] =
    DECLARATION_KINDS.filter(kind => !info.defs.exists(_.baseName == kind)).map(kind =>
      message(kind)
        .addOneofDecl(oneOf("v"))
        .addAllField(info.defs.filter(defn => info.supports(kind)(defn.baseName)).map(defn =>
          field(defn.baseName).setTypeName(Name(defn.baseName).ucamel).setOneofIndex(0).build()
        ).asJava)
        .build()
    )

  def families(): Seq[DescriptorProto] =
    info.families.filter(family => !info.defs.exists(_.baseName == family)).map(family =>
      message(family)
        .addOneofDecl(oneOf("v"))
        .addAllField(info.defs.filter(defn => info.supports(family)(defn.baseName)).map(defn =>
          field(defn.baseName).setTypeName(Name(defn.baseName).ucamel).setOneofIndex(0).build()
        ).asJava)
        .build()
    )

  def node(defn: ClassDef): DescriptorProto =
    message(defn.baseName)
      .addAllField(defn.params.map(param =>
        field(param.name.value, Some(param.decltpe.get))
          .build()
      ).asJava)
      .build()

  def declarations(): Seq[DescriptorProto] =
    info.defs.filter(defn => info.supports("Declaration")(defn.baseName)).map(node)

  def nodes(): Seq[DescriptorProto] =
    info.defs.filter(defn => info.supports("NodeFamily")(defn.baseName)).map(node)

  def renderType(field: FieldDescriptorProto): String = field.getType match {
    case PType.TYPE_INT64 => "int64"
    case PType.TYPE_BOOL => "bool"
    case PType.TYPE_STRING => "string"
    case PType.TYPE_BYTES => "bytes"
    case _ => field.getTypeName
  }

  def renderField(field: FieldDescriptorProto, idx: Int, inOneof: Boolean = false): String =
    "  " +
      (if (inOneof) "  " else "") +
      (if (!inOneof && field.getLabel == FieldDescriptorProto.Label.LABEL_REPEATED) "repeated " else "") +
      (if (!inOneof && field.getLabel == FieldDescriptorProto.Label.LABEL_REQUIRED) "required " else "") +
      (if (!inOneof && field.getLabel == FieldDescriptorProto.Label.LABEL_OPTIONAL) "optional " else "") +
      renderType(field) + " " +
      field.getName + " = " +
      (idx + 1).toString + ";"

  def renderFields(message: DescriptorProto): String =
    if(message.getOneofDeclCount == 0) {
      message.getFieldList.asScala.zipWithIndex.map { case (field, idx) => renderField(field, idx) }.mkString("\n")
    } else if(message.getOneofDeclCount == 1) {
      f"""  oneof ${message.getOneofDecl(0).getName} {
         |${message.getFieldList.asScala.zipWithIndex.map { case (field, idx) => renderField(field, idx, inOneof=true) }.mkString("\n")}
         |  }""".stripMargin
    } else ???

  def render(message: DescriptorProto): String = {
    f"""message ${message.getName} {
       |${renderFields(message)}
       |}""".stripMargin
  }

  def make(): Unit = {
    val descriptors =
      basicTypes() ++
        declarationKinds() ++
        families() ++
        declarations() ++
        nodes() ++
        boxedTypes.values

    val f = output.toPath.resolve("protobuf").resolve("col.proto").toFile
    val messages = descriptors.map(render).mkString("\n\n")
    writer(f,
      f"""syntax = "proto2";
         |package vct.col.serialize;
         |
         |$messages
         |""".stripMargin)
  }
}
