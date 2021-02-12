package viper.api

import java.io.{File, FileOutputStream, OutputStreamWriter}
import hre.lang.System.Warning
import viper.silicon.SiliconFrontend
import viper.silver.ast.{And, BinExp, DomainFuncApp, Exp, LocalVarDecl, Node, Or, Seqn, Typed}
import viper.silver.frontend.{SilFrontend, SilFrontendConfig}
import viper.silver.verifier.{AbstractError, NoVerifier, Verifier}

object DummyFrontend extends SilFrontend() {
  val NO_VERIFIER = new NoVerifier

  override def createVerifier(fullCmd: String): Verifier = NO_VERIFIER
  override def configureVerifier(args: Seq[String]): SilFrontendConfig = ???
}

/**
  * Compare trees of silver for differences. Used to determine differences between our silver output and the same silver
  * as it's parsed.
  */
object SilverTreeCompare {
  def syntacticSugarIssues(node: Node): Either[Seq[AbstractError], Seq[(Node, Node)]] = {
    val tmp = File.createTempFile("vercors-output-", ".sil")
    tmp.deleteOnExit()
    val writer = new OutputStreamWriter(new FileOutputStream(tmp))
    writer.write(node.toString)
    writer.close()

    val frontend = DummyFrontend
    frontend.init(DummyFrontend.NO_VERIFIER)
    frontend.reset(tmp.toPath)
    frontend.parsing()
    frontend.semanticAnalysis()
    frontend.translation()

    if(frontend.errors.nonEmpty) {
      Left(frontend.errors)
    } else {
      Right(compare(node, frontend.translationResult))
    }
  }

  private def subnodesWithoutType(node: Node): Seq[Node] = node match {
    case typ: Typed => subnodes(typ).init
    case other => subnodes(other)
  }

  private def subnodes(node: Node): Seq[Node] = node match {
    case DomainFuncApp(_, args, map) =>
      args ++ map.toSeq.sortBy(pair => pair._1.name).flatMap(pair => Seq(pair._1, pair._2))
    case other => other.subnodes
  }

  /**
    * Sequences are translated strangely by both vercors and viper. We put separate blocks for locals and statements,
    * so we end up with blocks with locals & 1 statement, and no locals and some statements. Viper translates
    * declarations by leaving an empty block in place of a declaration statement. The solution is to flatly collect
    * directly nested blocks.
    */
  def collectSeqn(seqn: Seqn): Seq[Node] =
    seqn.scopedDecls.collect {
      case local: LocalVarDecl => local
    } ++ seqn.ss.flatMap {
      case seqn: Seqn => collectSeqn(seqn)
      case other => Seq(other)
    }

  /**
    * Here due to this parsing detail:
    * (a && (b && c)).toString == "a && b && c"
    * parse("a && b && c") == ((a && b) && c)
    * This distinction is not interesting to report.
    */
  def collectFlatly[T <: Node](node: T): Seq[Node] = {
    subnodesWithoutType(node).flatMap {
      case child if child.getClass == node.getClass =>
        collectFlatly(child)
      case other => Seq(other)
    }
  }

  /**
    * Use a custom flattener (e.g. collectCons above) for a node, instead of .subnodes.
    */
  def compare[T <: Node](left: T, right: T, flattener: T => Seq[Node]): Seq[(Node, Node)] = {
    val leftChildren = flattener(left)
    val rightChildren = flattener(right)

    if (leftChildren.size != rightChildren.size) {
      Seq((left, right))
    } else {
      leftChildren.zip(rightChildren).flatMap {
        case (left, right) => compare(left, right)
      }
    }
  }

  def compare(left: Node, right: Node): Seq[(Node, Node)] = (left, right) match {
    case (left: Seqn, right: Seqn) =>
      compare(left, right, collectSeqn)
    case (left: And, right: And) =>
      compare(left, right, collectFlatly)
    case (left: Or, right: Or) =>
      compare(left, right, collectFlatly)
    case (left, right) if left.getClass != right.getClass =>
      Seq((left, right))
    case _ =>
      compare[Node](left, right, subnodes)
  }
}
