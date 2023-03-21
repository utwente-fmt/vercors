import ColDefs.DECLARATION_KINDS
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.{Type => PType}
import com.google.protobuf.DescriptorProtos._

import java.io.File
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.meta.{Type => SType}

case class ColProto(info: ColDescription, output: File, writer: (File, String) => Unit) {
  case class Name(orig: String) {
    private val upperIndices = orig.capitalize.zipWithIndex.collect { case (c, i) if c.isUpper || c == '_' => i } :+ orig.length
    private val unsafeParts = upperIndices.zip(upperIndices.tail).map {
      case (from, to) => orig.slice(from, to).toLowerCase.replace("_", "")
    }

    val parts: Seq[String] = Map(
      Seq("instance", "of") -> Seq("vct", "instance", "of"),
      Seq("class") -> Seq("vct", "class"),
      Seq("empty") -> Seq("vct", "empty"),
      Seq("assert") -> Seq("vct", "assert"),
    ).getOrElse(unsafeParts, unsafeParts)

    def snake: String = parts.mkString("_")
    def camel: String = parts.head + parts.tail.map(_.capitalize).mkString("")
    def ucamel: String = parts.map(_.capitalize).mkString("")
  }

  sealed trait Typ {
    def isPrimitive: Boolean = this match {
      case TBool | TRef() | TInt | TBigInt | TBigDecimal | TString => true
      case TName(_) => true
      case TOption(t) if t.isMarkable => true
      case TSeq(t) if t.isMarkable => true
      case TSet(t) if t.isMarkable => true
      case _ => false
    }

    def isMarkable: Boolean = this match {
      case TBool | TRef() | TInt | TBigInt | TBigDecimal | TString => true
      case TName(_) => true
      case _ => false
    }

    override def toString: String = this match {
      case TBool => "Bool"
      case TRef() => "Ref"
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
  case object TInt extends Typ
  case object TBigInt extends Typ
  case object TBigDecimal extends Typ
  case object TString extends Typ
  case class TRef()(val scalaArg: SType) extends Typ
  case class TName(name: String) extends Typ
  case class TOption(t: Typ)(val scalaArg: SType) extends Typ
  case class TSeq(t: Typ)(val scalaArg: SType) extends Typ
  case class TSet(t: Typ)(val scalaArg: SType) extends Typ
  case class TTuple(ts: Seq[Typ])(val scalaArg: Seq[SType]) extends Typ

  def getType(t: SType): Typ = t match {
    case SType.Apply(SType.Name("Seq"), List(SType.Name("ExpectedError"))) => TName("ExpectedErrors")

    case SType.Apply(SType.Name("Seq"), List(arg)) => TSeq(getType(arg))(arg)
    case SType.Apply(SType.Name("Option"), List(arg)) => TOption(getType(arg))(arg)
    case SType.Apply(SType.Name("Set"), List(arg)) => TSet(getType(arg))(arg)
    case SType.Tuple(ts) => TTuple(ts.map(getType))(ts)

    case SType.Apply(SType.Name(decl), List(SType.Name("G"))) if DECLARATION_KINDS.contains(decl) => TName(decl)
    case SType.Apply(SType.Name(node), List(SType.Name("G"))) if info.families.contains(node) => TName(node)

    case SType.Apply(SType.Name("Ref"), List(SType.Name("G"), decl)) => TRef()(decl)
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
    val name = TName(t.toString)
    val proto = message(name.name)
    boxedTypes.getOrElseUpdate(t, t match {
      case t @ TTuple(ts) =>
        boxedTypeTuple(name) = t
        proto.addAllField(ts.zipWithIndex.map {
          case (t, i) =>
            val f = field(f"v${i + 1}")
            setType(f, t)
            f.build()
        }.asJava).build()
      case other =>
        boxedTypeForward(name) = other
        val f = field("v")
        setType(f, t)
        proto.addField(f).build()
    })
    name
  }

  def primitivize(t: Typ): Typ = t match {
    case t if t.isPrimitive => t

    case TSeq(inner) if inner.isMarkable => t
    case TSet(inner) if inner.isMarkable => t
    case TOption(inner) if inner.isMarkable => t

    case st @ TSeq(t) => TSeq(box(t))(st.scalaArg)
    case st @ TSet(t) => TSet(box(t))(st.scalaArg)
    case st @ TOption(t) => TOption(box(t))(st.scalaArg)
    case other => box(other)
  }

  def setPrimitivizedType(builder: FieldDescriptorProto.Builder, t: Typ): Unit =
    t match {
      case TBool => builder.setType(PType.TYPE_BOOL)
      case TRef() => builder.setTypeName("Ref")
      case TInt => builder.setType(PType.TYPE_INT32)
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
      .addField(field("scale").setType(PType.TYPE_INT32))
      .addField(field("unscaledValue").setTypeName("BigInt"))
      .build(),
    message("Ref")
      .addField(field("index").setType(PType.TYPE_INT64))
      .build(),
    message("ExpectedErrors")
      .build(),
  )

  def declarationKinds(): Seq[DescriptorProto] =
    DECLARATION_KINDS.filter(kind => !info.defs.exists(_.baseName == kind)).map(kind => {
      boxedTypeFamily(TName(kind)) = kind
      message(kind)
        .addField(field("id").setType(PType.TYPE_INT64))
        .addOneofDecl(oneOf("v"))
        .addAllField(info.defs.filter(defn => info.supports(kind)(defn.baseName)).map(defn =>
          field(defn.baseName).setTypeName(Name(defn.baseName).ucamel).setOneofIndex(0).build()
        ).asJava)
        .build()
    })

  def families(): Seq[DescriptorProto] =
    info.families.filter(family => !info.defs.exists(_.baseName == family)).map(family => {
      boxedTypeFamily(TName(family)) = family
      message(family)
        .addOneofDecl(oneOf("v"))
        .addAllField(info.defs.filter(defn => info.supports(family)(defn.baseName)).map(defn =>
          field(defn.baseName).setTypeName(Name(defn.baseName).ucamel).setOneofIndex(0).build()
        ).asJava)
        .build()
    })

  def node(defn: ClassDef): DescriptorProto = {
    boxedTypeFamily(TName(defn.baseName)) = defn.baseName
    val msg = message(defn.baseName)

    if(DECLARATION_KINDS.contains(defn.baseName)) {
      // Singleton declaration, e.g. Variable
      msg.addField(field("id").setType(PType.TYPE_INT64))
    }

    msg
      .addAllField(defn.params.map(param =>
        field(param.name.value, Some(param.decltpe.get))
          .build()
      ).asJava)
      .build()
  }

  def declarations(): Seq[DescriptorProto] =
    info.defs.filter(defn => info.supports("Declaration")(defn.baseName)).map(node)

  def nodes(): Seq[DescriptorProto] =
    info.defs.filter(defn => info.supports("NodeFamily")(defn.baseName)).map(node)

  def renderType(field: FieldDescriptorProto): String = field.getType match {
    case PType.TYPE_INT64 => "int64"
    case PType.TYPE_INT32 => "int32"
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

  def renderFields(message: DescriptorProto): String = {
    val oneofs =
      for(i <- 0 until message.getOneofDeclCount)
        yield f"""  oneof ${message.getOneofDecl(i).getName} {
          |${message.getFieldList.asScala.zipWithIndex.collect {
              case (field, idx) if field.hasOneofIndex && field.getOneofIndex == i =>
                renderField(field, idx, inOneof = true) }.mkString("\n")}
          |  }""".stripMargin

    val normalFields =
      message.getFieldList.asScala.zipWithIndex.collect {
        case (field, idx) if !field.hasOneofIndex =>
          renderField(field, idx)
      }

    val flatNormalFields = if(normalFields.nonEmpty) Seq(normalFields.mkString("\n")) else Nil

    (flatNormalFields ++ oneofs).mkString("\n\n")
  }

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
