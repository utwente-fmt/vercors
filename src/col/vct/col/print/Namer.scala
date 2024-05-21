package vct.col.print

import hre.util.ScopedStack
import vct.col.ast._

import scala.collection.mutable

case class Namer[G](syntax: Ctx.Syntax) {
  private val stack = ScopedStack[Node[G]]()
  private val names = mutable.Map[(scala.Any, String, Int), Declaration[G]]()

  private val keywords: Set[String] = syntax match {
    case Ctx.PVL => Keywords.PVL
    case Ctx.Silver => Set()
    case Ctx.Java => Keywords.JAVA
    case Ctx.C => Keywords.C_CPP_GPGPU
    case Ctx.CPP => Keywords.C_CPP_GPGPU
    case Ctx.Cuda => Keywords.C_CPP_GPGPU
    case Ctx.OpenCL => Keywords.C_CPP_GPGPU
  }

  def nearest(f: PartialFunction[Node[G], Unit]): Seq[Node[G]] =
    stack.toSeq.filter(n => f.isDefinedAt(n))

  private def nearestClass = nearest {
    case _: Class[G] | _: JavaClass[G] | _: SeqProg[G] | _: JavaInterface[G] | _: JavaAnnotationInterface[G] => ()
  }

  private def nearestVariableScope = nearest {
    case _: ParBlock[G] => ()
    case _: VecBlock[G] => ()
    case _: CatchClause[G] => ()
    case _: Scope[G] => ()
    case _: SignalsClause[G] => ()
    case _: AxiomaticDataType[G] => ()
    case _: JavaClass[G] => ()
    case _: JavaInterface[G] => ()
    case _: Predicate[G] => ()
    case _: InstancePredicate[G] => ()
    case _: ModelProcess[G] => ()
    case _: ModelAction[G] => ()
    case _: ADTFunction[G] => ()
    case _: Function[G] => ()
    case _: Procedure[G] => ()
    case _: InstanceFunction[G] => ()
    case _: InstanceMethod[G] => ()
    case _: CodeStringQuantifierMethod[G] => ()
    case _: Constructor[G] => ()
    case _: JavaConstructor[G] => ()
    case _: JavaMethod[G] => ()
    case _: PVLConstructor[G] => ()
    case _: Forall[G] => ()
    case _: Starall[G] => ()
    case _: Exists[G] => ()
    case _: Sum[G] => ()
    case _: Product[G] => ()
    case _: Let[G] => ()
    case _: ScopedExpr[G] => ()
    case _: ForPerm[G] => ()
  }

  private def nearestCallable = nearest {
    case _: Function[G] => ()
    case _: Procedure[G] => ()
    case _: InstanceFunction[G] => ()
    case _: InstanceMethod[G] => ()
    case _: CodeStringQuantifierMethod[G] => ()
    case _: Constructor[G] => ()
    case _: JavaMethod[G] => ()
    case _: JavaAnnotationMethod[G] => ()
    case _: JavaConstructor[G] => ()
    case _: PVLConstructor[G] => ()
    case _: CFunctionDefinition[G] => ()
    case _: CPPFunctionDefinition[G] => ()
  }

  def unpackName(name: String): (String, Int) = {
    val m = "^(.*?)([1-9][0-9]*)?$".r.findFirstMatchIn(name).get
    if (Option(m.group(2)).isDefined) {
      (m.group(1), Integer.parseInt(m.group(2)))
    } else {
      (m.group(1), 0)
    }
  }

  def packName(name: String, index: Int): String =
    if (index == 0) name
    else s"$name$index"

  def nameKeyed(keys: Seq[scala.Any], decl: Declaration[G]): Unit = {
    if(keys.isEmpty) {
      // Refuse to name this declaration, it is unclear how to distinguish them.
      return
    }

    val name = decl.o.getPreferredNameOrElse()
    var (baseName, index) = unpackName(decl match {
      case declaration: GlobalDeclaration[_] => declaration match {
        case _: Applicable[_] => name.camel
        case _ => name.ucamel
      }
      case constant: EnumConstant[_] => name.usnake
      case decl: LabelDecl[_] => name.usnake
      case _ => name.camel
    })

    if(index == 0 && keywords.contains(baseName)) {
      index = 1
    }

    while(keys.exists(key => names.contains((key, baseName, index)))) {
      val similar_keys = keys.collect(key => names.get((key, baseName, index)))
      index += 1
    }

    names((keys.head, baseName, index)) = decl
  }

  def name(node: Node[G]): Unit = {
    stack.having(node) { node.subnodes.foreach(name) }

    node match {
      case decl: GlobalDeclaration[G] => nameKeyed(nearest { case _: Program[G] => () }, decl)
      case decl: ClassDeclaration[G] => nameKeyed(nearestClass, decl)
      case decl: ADTDeclaration[G] => nameKeyed(if(syntax == Ctx.Silver) Seq(3) else nearest { case _: AxiomaticDataType[G] => () }, decl)
      case decl: ModelDeclaration[G] => nameKeyed(nearest { case _: Model[G] => () }, decl)
      case decl: EnumConstant[G] => nameKeyed(nearest { case _: Enum[G] => () }, decl)
      case decl: Variable[G] => nameKeyed(nearestVariableScope, decl)
      case decl: LabelDecl[G] => nameKeyed(nearestCallable, decl)
      case decl: SendDecl[G] => nameKeyed(nearest { case _: ParBlock[G] | _: Loop[G] => () }, decl)
      case decl: ParBlockDecl[G] => nameKeyed(nearest { case _: ParBlock[G] => () }, decl)
      case decl: ParInvariantDecl[G] => nameKeyed(nearest { case _: ParInvariant[G] => () }, decl)
      case decl: CLocalDeclaration[G] => nameKeyed(nearestVariableScope, decl)
      case decl: CParam[G] => nameKeyed(nearestCallable, decl)
      case decl: CPPLocalDeclaration[G] => nameKeyed(nearestVariableScope, decl)
      case decl: CPPParam[G] => nameKeyed(nearestCallable, decl)
      case decl: JavaLocalDeclaration[G] => nameKeyed(nearestCallable, decl)
      case decl: Endpoint[G] => nameKeyed(nearest { case _: SeqProg[G] => () }, decl)
      case decl: JavaParam[G] => nameKeyed(nearestCallable, decl)
      case _ =>
    }
  }

  def finish: Map[Declaration[G], String] =
    Map(names.map { case (_, name, index) -> decl => decl -> packName(name, index) }.toSeq: _*)
}
