package vct.main.modes

import vct.col.ast.{
  Applicable,
  ByReferenceClass,
  Declaration,
  Node,
  Program,
  Verification,
  VerificationContext,
}
import vct.col.origin.{BlameCollector, SourceName}
import vct.col.print.{Doc, Text}
import vct.col.ref.Ref
import vct.col.rewrite.Generation
import vct.main.stages.{Parsing, Resolution}
import vct.options.Options
import vct.parsers.ParseResult
import vct.parsers.transform.ConstantBlameProvider

object ColPrinter {
  sealed trait Format
  object Format {
    case object PreCol extends Format
    case object CoreCol extends Format
  }

  object cls {
    val Node = classOf[Node[_]]
    val Seq = classOf[Seq[_]]
    val Product = classOf[Product]
    val Option = classOf[Option[_]]
    val Ref = classOf[Ref[_, _]]
  }

  def isNode(obj: Object): Boolean =
    obj match {
      case null => true
      case c: Predef.Class[_] => c == cls.Node
      case _ => cls.Node.isAssignableFrom(obj.getClass)
    }

  def isRef(obj: Object): Boolean =
    obj match {
      case null => true
      case c: Predef.Class[_] => c == cls.Ref
      case _ => cls.Ref.isAssignableFrom(obj.getClass)
    }

  def isSeq(obj: Object): Boolean =
    obj match {
      case null => true
      case c: Predef.Class[_] => c == cls.Seq
      case _ => cls.Seq.isAssignableFrom(obj.getClass)
    }

  def isProduct(obj: Object): Boolean =
    obj match {
      case null => true
      case c: Predef.Class[_] => c == cls.Product
      case _ =>
        cls.Product.isAssignableFrom(obj.getClass) &&
        (classOf[Tuple1[_]].isAssignableFrom(obj.getClass) ||
          classOf[Tuple2[_, _]].isAssignableFrom(obj.getClass))
    }

  def isOption(obj: Object): Boolean =
    obj match {
      case null => true
      case c: Predef.Class[_] => c == cls.Option
      case _ => cls.Option.isAssignableFrom(obj.getClass)
    }

  def isVolatile(field: java.lang.reflect.Field): Boolean =
    java.lang.reflect.Modifier.isVolatile(field.getModifiers)

  def splitFields(o: Object): Seq[(String, Object)] =
    o.getClass.getDeclaredFields.filter(field => !isVolatile(field)).map(f => {
      f.setAccessible(true)
      val v = f.get(o)
      (f.getName, v)
    }).collect {
      case (f, v)
          if (isNode(v) || isSeq(v) || isOption(v) || isProduct(v) ||
            isRef(v) || v.getClass.isPrimitive) && f != "debugRewriteState" &&
            f != "o" =>
        (f, v)
    }

  implicit val ctx = vct.col.print.Ctx(width = 200)
  import vct.col.print.Line

  def colPrint(o: Object): Doc =
    o match {
      case null => Text("null")
      case ParseResult(decls, _) =>
        Text("### ParseResult ###") <+/> Doc.stack2(decls.map(colPrint)) <+/>
          "### ParseResult end ###"
      case Verification(tasks, _) =>
        Doc.fold(tasks.map(colPrint))((l, r) => l <> Line <> Line <> Line <> r)
      case VerificationContext(prog) =>
        Text("### Program ###") <+/> colPrint(prog) <+/> "### Program end ###"
      case Program(decls) => Doc.stack2(decls.map(colPrint))
      case r: Ref[_, _] => Text("(ref)")
      case xs: Seq[_] =>
        Text("Seq(") <> Doc.args(xs.map { case o: Any =>
          colPrint(o.asInstanceOf[Object])
        }) <> ")"
      case Some(o) => Text("Some(") <> colPrint(o.asInstanceOf[Object]) <> ")"
      case None => Text("None")
      case d: Declaration[_] => name(d) <+> "@" <+> classSplitFields(d)
      case o: Object => classSplitFields(o)
    }

  def name(d: Node[_]): Doc =
    d.o.originContents match {
      case SourceName(name) +: _ => Text(name)
      case _ => Text(d.o.getPreferredNameOrElse().camel)
    }

  def classSplitFields(o: Object): Doc =
    Text(o.getClass.getSimpleName) <> "(" <> Doc.args(splitFields(o).map {
      case (f, v) => colPrint(v)
    }) <> ")"

  def runOptions(options: Options): Int = {
    val collector = BlameCollector()
    val blameProvider = ConstantBlameProvider(collector)
    val parsing = Parsing.ofOptions[Generation](options, blameProvider)
    val parseResult = parsing.run(options.inputs)

    if (options.format == ColPrinter.Format.PreCol) {
      println(colPrint(parseResult).toStringWithContext)
      return 0
    }

    if (options.format == ColPrinter.Format.CoreCol) {
      val resolution = Resolution.ofOptions[Generation](options, blameProvider)
      val resolutionResult = resolution.run(parseResult)
      println(colPrint(resolutionResult).toStringWithContext)
      return 0
    }

    0
  }
}
