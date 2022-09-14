import ColDefs._

import java.io.File
import java.nio.file.Files
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.meta._

/**
 * object Namespace {
 *  case class Thing(param1: Int)(val blame: Blame)(implicit val o: Origin) extends Node
 * }
 *
 * becomes:
 * ClassDef(Seq("Namespace", "Thing"), List(param1: Int), Some(Blame), List(case))
 */
case class ClassDef(names: Seq[String], params: List[Term.Param], blameType: Option[Type], mods: List[Mod]) {
  for(param <- params) {
    // PB: sorry if this breaks; can't find the list of keywords elsewhere...
    if(internal.tokenizers.keywords.contains(param.name.value)) {
      MetaUtil.fail(
        s"Class ${names.mkString(".")} has a parameter named ${param.name.value}, which is a keyword in Scala.\n" +
          "Although this is possible, this leads to incorrectly generated patterns in match statements, so please pick a different name.\n" +
          "(The keyword may be a soft keyword, or only a keyword as of Scala 3, but will be generated incorrectly nonetheless.)",
        node=Some(param)
      )
    }
  }

  def baseName: String = names.last
  def qualifiedName: String = names.mkString(".")

  def term: Term =
    if(names.size == 1) Term.Name(baseName)
    else Term.Select(termQual, Term.Name(baseName))

  def typ: Type =
    if(names.size == 1) Type.Name(baseName)
    else Type.Select(termQual, Type.Name(baseName))

  def rewriteHelperName: Type.Name =
    Type.Name("Rewrite" + qualifiedName)

  def rewriteBuilderName: Type.Name =
    Type.Name(qualifiedName + "Builder")

  private def termQual: Term.Ref = {
    val qualNames = names.init.map(Term.Name(_))
    qualNames.init.foldLeft[Term.Ref](qualNames.last)(Term.Select(_, _))
  }

  /**
   * Make a term that constructs this class.
   */
  def make(args: List[Term], blame: Term, origin: Term): Term =
    if(mods.collectFirst{ case Mod.Case() => () }.nonEmpty)
      blameType match {
        case Some(_) => q"$term(..$args)($blame)($origin)"
        case None => q"$term(..$args)($origin)"
      }
    else
      blameType match {
        case Some(_) => q"new $typ(..$args)($blame)($origin)"
        case None => q"new $typ(..$args)($origin)"
      }
}

class ColDescription {
  val defs: ArrayBuffer[ClassDef] = ArrayBuffer()
  val bases: mutable.Map[String, List[String]] = mutable.Map()
  val families: mutable.Set[String] = mutable.Set()

  def supports(baseType: String)(cls: String): Boolean = {
    baseType == cls || bases.getOrElse(cls, Seq()).exists(supports(baseType))
  }

  /**
   * Provides the default way to rewrite a term with a fixed type
   */
  def rewriteDefault(term: Term, typ: Type): Term = typ match {
    case Type.Apply(Type.Name("Seq"), List(Type.Apply(Type.Name(declKind), List(Type.Name("G"))))) if DECLARATION_KINDS.contains(declKind) =>
      q"rewriter.${ColDefs.scopes(declKind)}.dispatch($term)"
    case Type.Apply(Type.Name(collectionType), List(arg)) if Set("Seq", "Set", "Option").contains(collectionType) =>
      q"$term.map(element => ${rewriteDefault(q"element", arg)})"

    case Type.Tuple(List(t1, t2)) =>
      q"(${rewriteDefault(q"$term._1", t1)}, ${rewriteDefault(q"$term._2", t2)})"
    case Type.Tuple(List(t1, t2, t3)) =>
      q"(${rewriteDefault(q"$term._1", t1)}, ${rewriteDefault(q"$term._2", t2)}, ${rewriteDefault(q"$term._3", t3)})"
    case Type.Tuple(other) =>
      MetaUtil.fail(s"Oops, this tuple is too long for me! size=${other.size}", node=Some(typ))

    case Type.Apply(Type.Name(declKind), List(Type.Name("G"))) if DECLARATION_KINDS.contains(declKind) =>
      q"rewriter.${ColDefs.scopes(declKind)}.dispatch($term)"
    case Type.Apply(Type.Name(typ), List(Type.Name("G"))) if families.contains(typ) =>
      q"rewriter.dispatch($term)"

    case Type.Apply(Type.Name("Ref"), List(_, Type.Apply(decl @ Type.Name(tDecl), _))) =>
      if(ColDefs.DECLARATION_KINDS.exists(kind => supports(kind)(tDecl)))
        q"rewriter.porcelainRefSucc[$decl[Post]]($term).getOrElse(rewriter.succ[${Type.Name(tDecl)}[Post]]($term.decl))"
      else
        q"rewriter.porcelainRefSucc[$decl[Post]]($term).getOrElse(rewriter.anySucc[${Type.Name(tDecl)}[Post]]($term.decl))"
    case Type.Name("Int") | Type.Name("String") | Type.Name("Boolean") | Type.Name("BigInt") | Type.Apply(Type.Name("Referrable"), List(Type.Name("G"))) | Type.Name("ExpectedError") =>
      term
    case Type.Apply(Type.Name("Either"), List(t1, t2)) =>
      q"$term.left.map(l => ${rewriteDefault(q"l", t1)}).map(r => ${rewriteDefault(q"r", t2)})"
    case _ =>
      MetaUtil.fail(
        s"Encountered an unknown type while generating default rewriters: $typ\n" +
          "Perhaps there is an 'extends Expr' or so missing, or ColDefs.DECLARATION_KINDS is incomplete?",
        node=Some(typ)
      )
  }

  /**
   * Collect the shape of a node. Nodes are shaped either like:
   * (case) class(param: T, ...)(implicit o: Origin)
   * or:
   * (case) class(param: T, ...)(val blame: BT)(implicit o: Origin)
   * We need to remember the modifier ("case") to know how to construct the node.
   * Some nodes are within an object, so we remember the chain in the path, e.g.:
   * object Constants {
   *   class Test()(implicit val o: Origin)
   * }
   * We would need the path Seq("Constants").
   */
  def collectNode(path: Seq[String])(stat: Stat): Unit = stat match {
    case Defn.Class(
    mods, name, List(Type.Param(_, Type.Name("G"), _, _, _, _)),
    Ctor.Primary(_, _, parameterLists), _)
      if mods.collectFirst { case Mod.Abstract() => () }.isEmpty && Set(2, 3).contains(parameterLists.size) =>
      val originList = if(parameterLists.size == 2) parameterLists(1) else parameterLists(2)
      originList match {
        case List(Term.Param(List(scala.meta.Mod.Implicit(), scala.meta.Mod.ValParam()), Name("o"), Some(scala.meta.Type.Name("Origin")), _)) =>
          if(parameterLists.size == 2) {
            defs += ClassDef(path :+ name.value, parameterLists.head, blameType=None, mods)
          } else {
            parameterLists(1) match {
              case List(Term.Param(List(Mod.ValParam()), Name("blame"), Some(t@Type.Apply(_, _)), _)) =>
                defs += ClassDef(path :+ name.value, parameterLists.head, Some(t), mods)
            }
          }
        case _ =>
      }
    case otherCls: Defn.Class if otherCls.mods.collectFirst { case Mod.Abstract() => () }.isEmpty =>
      MetaUtil.fail("Could not parse the following class. Is the class in the right format?", node = Some(otherCls))
    case Defn.Object(_, name, Template(_, _, _, stats)) =>
      stats.foreach(collectNode(path :+ name.value))
    case _ =>
  }

  /**
   * Collect all the base classes and traits of every definition. They are remembered by base name only, and hence
   * nodes may not have overlapping names.
   */
  def collectBases(stat: Stat): Unit = stat match {
    case Defn.Class(_, name, _, _, Template(_, inits, _, _)) =>
      bases(name.value) = inits.collect {
        case Init(Type.Name(name), _, _) => name
        case Init(Type.Apply(Type.Name(name), _), _, _) => name
      }
    case Defn.Trait(_, name, _, _, Template(_, inits, _, _)) =>
      bases(name.value) = inits.collect {
        case Init(Type.Name(name), _, _) => name
        case Init(Type.Apply(Type.Name(name), _), _, _) => name
      }
    case Defn.Object(_, _, Template(_, _, _, stats)) =>
      stats.foreach(collectBases(_))
    case _ =>
  }

  /**
   * Collect node families: the root of a rewriting class. Nodes in a family always rewrite to a type in that family.
   */
  def collectFamily(stat: Stat): Unit = stat match {
    case Defn.Class(_, name, _, _, Template(_, inits, _, _)) =>
      if(inits.collectFirst {
        case Init(Type.Apply(Type.Name("NodeFamily"), List(Type.Name("G"))), _, _) => ()
      }.nonEmpty)
        families += name.value
    case Defn.Trait(_, name, _, _, Template(_, inits, _, _)) =>
      if(inits.collectFirst {
        case Init(Type.Apply(Type.Name("NodeFamily"), List(Type.Name("G"))), _, _) => ()
      }.nonEmpty)
        families += name.value
    case Defn.Object(_, _, Template(_, _, _, stats)) =>
      stats.foreach(collectFamily(_))
    case _ =>
  }

  /**
   * Parse a scala file, and collect the nodes, hierarchy and node families.
   */
  def collectInfo(file: File): Unit = {
    val text = new String(Files.readAllBytes(file.toPath), "UTF-8")
    val input = Input.VirtualFile(file.toString, text)
    input.parse[Source].get match {
      case Source(List(Pkg(_, stats))) =>
        stats.foreach(collectNode(Seq()))
        stats.foreach(collectBases)
        stats.foreach(collectFamily)
      case other =>
        MetaUtil.fail(
          s"Source file $file did not parse in the expected pattern Source(List(Pkg(_, stats))), but instead as:\n" +
            other.toString,
          node=Some(other)
        )
    }
  }
}
