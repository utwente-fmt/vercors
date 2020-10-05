package viper.api

import java.io.{File, FileOutputStream, OutputStreamWriter}

import hre.lang.System.Warning
import viper.silicon.SiliconFrontend
import viper.silver.ast.{And, Exp, LocalVarDecl, Node, Seqn}
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
  def collectCons(and: And): Seq[Node] =
    (and.left match {
      case and: And => collectCons(and)
      case other => Seq(other)
    }) ++ (and.right match {
      case and: And => collectCons(and)
      case other => Seq(other)
    })

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
      compare(left, right, collectCons)
    case _ =>
      compare[Node](left, right, _.subnodes)
  }
}
