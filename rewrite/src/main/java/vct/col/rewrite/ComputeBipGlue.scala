package vct.col.rewrite

import com.typesafe.scalalogging.LazyLogging
import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.{BasicLogManager, LogManager}
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import org.sosy_lab.java_smt.api.SolverContext.ProverOptions
import vct.col.ast.{BipData, BipGlue, BipGlueAccepts, BipGlueDataWire, BipGlueRequires, BipIncomingData, BipOutgoingData, BipPort, BipTransition, Class, ClassDeclaration, Declaration, Node, Program}
import vct.col.ref.Ref
import org.sosy_lab.java_smt.api.{BooleanFormula, BooleanFormulaManager, SolverContext}

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable
import scala.util.Using

case object ComputeBipGlue extends RewriterBuilder {
  override def key: String = "computeGlue"
  override def desc: String = "Encodes BIP glue into SAT to compute all possible synchronizations"
}

case class ComputeBipGlue[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  var program: Program[Pre] = _

  implicit class BooleanFormulaManagerBuildHelpers[G](bm: BooleanFormulaManager) {
    def xorAll(bs: BooleanFormula*): BooleanFormula = bs match {
      case Seq() => bm.makeTrue()
      case Seq(b) => b
      case b +: bs => bm.xor(b, xorAll(bs: _*))
    }
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    this.program = program

    rewriteDefault(program)
  }

  case class BipSmt(ctx: SolverContext) {
    val names: mutable.Set[String] = mutable.Set()
    val nodeToName: mutable.Map[Node[Pre], String] = mutable.Map()

    val fm = ctx.getFormulaManager
    val bm = fm.getBooleanFormulaManager

    def prefix(n: Node[Pre]): String = n match {
      case _: BipTransition[Pre] => "t!"
      case _: BipData[Pre] => "d!"
      case _: BipPort[Pre] => "p!"
      case _: BipGlueDataWire[Pre] => "w!"
    }

    def unique(n: Node[Pre]): String = {
      val candidate = prefix(n) + n.o.preferredName
      var count: Option[Int] = None
      while (true) {
        val currentCandidate = count match {
          case Some(n) => s"$candidate$n"
          case None => candidate
        }
        if (names.contains(currentCandidate)) {
          count = count match {
            case Some(n) => Some(n + 1)
            case None => Some(0)
          }
        } else {
          names.add(currentCandidate)
          return currentCandidate
        }
      }
      ???
    }

    def name(n: Node[Pre]): String = nodeToName.getOrElseUpdate(n, unique(n))
    def get(n: Node[Pre]): BooleanFormula = {
      n match { case _: BipTransition[Pre] | _: BipGlueDataWire[Pre] | _: BipPort[Pre] | _: BipData[Pre] => }
      bm.makeVariable(name(n))
    }
  }

  sealed trait Constraint {
    def toSmt2(ctx: BipSmt): BooleanFormula
  }
  case class PortEnablesInputsOutputs(port: BipPort[Pre], datas: Seq[BipData[Pre]]) extends Constraint {
    override def toSmt2(ctx: BipSmt): BooleanFormula = {
      ctx.bm.implication(
        ctx.get(port),
        ctx.bm.and(datas.map(ctx.get) :_*)
      )
    }
  }
  case class OutputDataNeedsPort(data: BipOutgoingData[Pre], ports: Seq[BipPort[Pre]]) extends Constraint {
    override def toSmt2(ctx: BipSmt): BooleanFormula = {
      ctx.bm.implication(
        ctx.get(data),
        ctx.bm.or(ports.map(ctx.get): _*)
      )
    }
  }
  case class InputRequiresOneActiveWire(data: BipIncomingData[Pre], incomingWires: Seq[BipGlueDataWire[Pre]]) extends Constraint {
    override def toSmt2(ctx: BipSmt): BooleanFormula = {
      val bm = ctx.bm
      bm.implication(
        ctx.get(data),
        bm.xorAll(incomingWires.map { wire =>
          bm.and(
            ctx.get(wire.dataOut.decl),
            ctx.get(wire)
          )
        }: _*)
      )
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case glue: BipGlue[Pre] =>
      val relevantDecls: Set[ClassDeclaration[Pre]] = glue.transSubnodes.collect {
        case BipGlueRequires(Ref(port), requires) => port +: requires.map(_.decl)
        case BipGlueAccepts(Ref(port), accepts) => port +: accepts.map(_.decl)
        case BipGlueDataWire(Ref(from), Ref(to)) => Seq(from, to)
      }.flatten.toSet

      // Collect all relevant components. It is assumed that if a component is not mentioned in the glue, we can ignore it for computing glue
      val classes = program.transSubnodes.collect {
        case cls: Class[Pre] if cls.declarations.exists(relevantDecls.contains) => cls
      }
      val ports = classes.flatMap { cls => cls.declarations.collect {
        case port: BipPort[Pre] => port
      }}
      val datas = classes.flatMap { cls => cls.declarations.collect {
        case data: BipData[Pre] => data
      }}

      val portEnablesInputsOutputs: Seq[Constraint] = classes.flatMap { cls =>
        val ports = cls.declarations.collect { case p: BipPort[Pre] => p }
        val datas = cls.declarations.collect { case d: BipData[Pre] => d }
        ports.map(PortEnablesInputsOutputs(_, datas))
      }

      val outputDataNeedsPort: Seq[Constraint] = classes.flatMap { cls =>
        val datas = cls.declarations.collect { case d: BipOutgoingData[Pre] => d }
        val ports = cls.declarations.collect { case p: BipPort[Pre] => p }
        // TODO: We ignore that some datas can only be output in the case of a few specific ports, instead of all ports in the class
        datas.map(OutputDataNeedsPort(_, ports))
      }

      val wires: Seq[BipGlueDataWire[Pre]] = glue.transSubnodes.collect { case w: BipGlueDataWire[Pre] => w }
      // We assume there are no duplicate wires - I think this might break the computation at the end of the below block
      assert(wires.length == wires.toSet.size)
      val inputRequiresOneActiveWire: Seq[Constraint] = {
        val incomingDatas = wires.map { wire => wire.dataIn.decl }
        incomingDatas.map { dataIn =>
          val wiresToDataIn = wires.filter(_.dataIn.decl == dataIn)
          InputRequiresOneActiveWire(dataIn, wiresToDataIn)
        }
      }

      val constraints = portEnablesInputsOutputs ++ outputDataNeedsPort ++ inputRequiresOneActiveWire

      Using(SolverContextFactory.createSolverContext(Solvers.SMTINTERPOL)) { ctx =>
        // Solve formula, get model, and print variable assignment
        Using(ctx.newProverEnvironment(ProverOptions.GENERATE_MODELS)) { prover =>
          val bipSmt = BipSmt(ctx)

          constraints.map(_.toSmt2(bipSmt)).foreach(prover.addConstraint)

          val isUnsat = prover.isUnsat()
          logger.info(s"isUnsat: $isUnsat")
          if (!isUnsat) {
            Using(prover.getModel()) { model =>
//              val enabledPorts = ports.filter { port => model.evaluate(bipSmt.get(port)) }
//              val wires = ???
              logger.info(model.toString)
            }
          }
        }
      }



      ???

    case other => rewriteDefault(other)
  }
}
