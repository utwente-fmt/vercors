package vct.col.rewrite.bip

import com.typesafe.scalalogging.LazyLogging
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import org.sosy_lab.java_smt.api.SolverContext.ProverOptions
import org.sosy_lab.java_smt.api.{BooleanFormula, BooleanFormulaManager, SolverContext}
import org.sosy_lab.java_smt.utils.PrettyPrinter
import vct.col.ast.{BipData, BipGlue, BipGlueAccepts, BipGlueDataWire, BipGlueRequires, BipIncomingData, BipOutgoingData, BipPort, BipPortSynchronization, BipTransition, Class, ClassDeclaration, Declaration, Expr, Node, Program}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Using

case object ComputeBipGlue extends RewriterBuilder {
  override def key: String = "computeGlue"
  override def desc: String = "Encodes BIP glue into SAT to compute all possible synchronizations"
}

//noinspection JavaAccessorEmptyParenCall
case class ComputeBipGlue[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  var program: Program[Pre] = _

  def xor(bm: BooleanFormulaManager, bs: Seq[BooleanFormula]): BooleanFormula = bs match {
    case Seq(b) => b
    case b +: bs => bm.xor(b, xor(bm, bs))
  }

  override def dispatch(program: Program[Pre]): Program[Post] = {
    this.program = program
    rewriteDefault(program)
  }

  case class BipSmt(ctx: SolverContext) {
    val names: mutable.Set[String] = mutable.LinkedHashSet()
    val nodeToName: mutable.Map[Node[Pre], String] = mutable.LinkedHashMap()

    val fm = ctx.getFormulaManager
    val bm = fm.getBooleanFormulaManager

    def prefix(n: Node[Pre]): String = n match {
      case _: BipTransition[Pre] => "t!"
      case _: BipData[Pre] => "d!"
      case _: BipPort[Pre] => "p!"
      case _: BipGlueDataWire[Pre] => "w!"
      case _ => ???
    }

    def unique(n: Node[Pre]): String = uniqueName(prefix(n) + n.o.preferredName)

    def uniqueName(candidate: String): String = {
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
      n match {
        case _: BipTransition[Pre] | _: BipGlueDataWire[Pre] | _: BipPort[Pre] | _: BipData[Pre] =>
        case _ => ???
      }
      bm.makeVariable(name(n))
    }
  }

  sealed trait Constraint {
    def toBooleanFormula(ctx: BipSmt): BooleanFormula
  }

  case class PortEnablesInputsOutputs(port: BipPort[Pre], datas: Seq[BipData[Pre]]) extends Constraint {
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula =
      ctx.bm.implication(
        ctx.get(port),
        ctx.bm.and(datas.map(ctx.get) :_*)
      )
  }

  case class OutputDataNeedsPort(data: BipOutgoingData[Pre], ports: Seq[BipPort[Pre]]) extends Constraint {
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula =
      ctx.bm.implication(
        ctx.get(data),
        ctx.bm.or(ports.map(ctx.get): _*)
      )
  }

  case class InputDataNeedsPort(data: BipIncomingData[Pre], ports: Seq[BipPort[Pre]]) extends Constraint {
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula =
      ctx.bm.implication(ctx.get(data), ctx.bm.or(ports.map(ctx.get(_)): _*))
  }

  case class InputRequiresOneActiveWire(data: BipIncomingData[Pre], incomingWires: Seq[BipGlueDataWire[Pre]]) extends Constraint {
    // We require at least one wire to exist for an input - otherwise the model is not well-formed
    // Otherwise, the resulting constraint is: data => xor(Seq()) === data => false === !data
    // Meaning, if there is no incoming wire, an input data can never be turned on.
    assert(incomingWires.nonEmpty)
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula = {
      val bm = ctx.bm
      bm.implication(
        ctx.get(data),
        xor(bm, incomingWires.map { wire =>
          bm.and(
            ctx.get(wire.dataOut.decl),
            ctx.get(wire)
          )
        })
      )
    }
  }

  case class WireRequiresPorts(wire: BipGlueDataWire[Pre]) extends Constraint {
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula =
      ctx.bm.implication(
        ctx.get(wire),
        ctx.bm.and(
          ctx.get(wire.dataOut.decl),
          ctx.get(wire.dataIn.decl)
        ))
  }

  case class Requires(requires: Seq[BipGlueRequires[Pre]]) extends Constraint {
    // There should be > 0 clauses, and all clauses should concern the same port
    assert(requires.forall(requires.head.port.decl == _.port.decl))

    val port = requires.head.port.decl
    lazy val others = requires.flatMap(_.others).map(_.decl)
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula = others match {
      case Seq() => ctx.bm.makeTrue()
      case _ =>
        ctx.bm.implication(
        ctx.get(port),
        ctx.bm.or(others.map(ctx.get): _*)
      )
    }
  }

  case class Accepts(accepts: Seq[BipGlueAccepts[Pre]], allPorts: Seq[BipPort[Pre]]) extends Constraint {
    // There should be > 0 clauses, and all clauses should concern the same port
    assert(accepts.forall(accepts.head.port.decl == _.port.decl))

    val port = accepts.head.port.decl
    lazy val others = accepts.flatMap(_.others).map(_.decl).toSet
    lazy val offPorts = allPorts.collect { case p if !others.contains(p) && p != port => p }

    override def toBooleanFormula(ctx: BipSmt): BooleanFormula =
      ctx.bm.implication(
        ctx.get(port),
        ctx.bm.and(offPorts.map { p => ctx.bm.not(ctx.get(p)) }: _*)
      )
  }

  case class ExcludeEmptySynchronization(allPorts: Seq[BipPort[Pre]]) extends Constraint {
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula = {
      // At least one of the ports should go off
      ctx.bm.or(allPorts.map(ctx.get): _*)
    }
  }

  def computeGlue(glue: BipGlue[Pre]): Seq[BipPortSynchronization[Pre]] = {
    val relevantDecls: Set[ClassDeclaration[Pre]] = glue.transSubnodes.collect {
      case BipGlueRequires(Ref(port), requires) =>
        port +: requires.map(_.decl)
      case BipGlueAccepts(Ref(port), accepts) => port +: accepts.map(_.decl)
      case BipGlueDataWire(Ref(from), Ref(to)) => Seq(from, to)
    }.flatten.toSet

    // Collect all relevant components. It is assumed that if a component is not mentioned in the glue, we can ignore it for computing glue
    val classes = program.collect {
      case cls: Class[Pre] if cls.declarations.exists(relevantDecls.contains) => cls
    }.toIndexedSeq
    val ports = classes.flatMap { cls => cls.declarations.collect { // TODO: Use common one for ports
      case port: BipPort[Pre] => port
    }}

    val portEnablesInputsOutputs: Seq[Constraint] = classes.flatMap { cls =>
      val ports = cls.declarations.collect { case p: BipPort[Pre] => p }
      // There might actually be multiple transitions given a port. However, these should be guaranteed to all have the same input datas.
      // This is because you can put multiple @Transition annotations on a method given a port, but not @Transition annotations for one
      // specific port on many different methods. (According to Larisa)
      val transitions = cls.declarations.collect { case t: BipTransition[Pre] => t.port.decl -> t }.toMap
      val outgoingDatas = cls.declarations.collect { case d: BipOutgoingData[Pre] => d }
      ports.map { port =>
        val incomingDatas = transitions(port).data.map(_.decl)
        PortEnablesInputsOutputs(port, outgoingDatas ++ incomingDatas)
      }
    }

    val outputDataNeedsPort: Seq[Constraint] = classes.flatMap { cls =>
      val datas = cls.declarations.collect { case d: BipOutgoingData[Pre] => d }
      val ports = cls.declarations.collect { case p: BipPort[Pre] => p }
      // TODO: We ignore that some datas can only be output in the case of a few specific ports, instead of all ports in the class
      datas.map(OutputDataNeedsPort(_, ports))
    }
    val inputDataNeedsPort: Seq[Constraint] = classes.flatMap { cls =>
      val transitions = cls.declarations.collect { case t: BipTransition[Pre] => t }
      val inDatas = transitions.flatMap(_.data.map(_.decl))
      inDatas.map { data =>
        val ports = transitions.collect { case t if t.data.map(_.decl).contains(data) => t.port.decl }
        InputDataNeedsPort(data, ports)
      }
    }

    val wires = glue.collect { case w: BipGlueDataWire[Pre] => w }.toIndexedSeq
    // We assume there are no duplicate wires - I think this might break the computation at the end of the below block
    assert(wires.length == wires.toSet.size)
    val inputRequiresOneActiveWire: Seq[Constraint] = {
      val incomingDatas = wires.map { wire => wire.dataIn.decl }
      incomingDatas.map { dataIn =>
        val wiresToDataIn = wires.filter(_.dataIn.decl == dataIn)
        InputRequiresOneActiveWire(dataIn, wiresToDataIn)
      }
    }
    val wireRequiresPorts: Seq[Constraint] = wires.map(WireRequiresPorts)

    val requires = {
      val requiresPorts = glue.requires.map(_.port.decl).distinct
      requiresPorts.map { port => Requires(glue.requires.filter(port == _.port.decl)) }
    }
    val accepts = {
      val acceptsPorts = glue.accepts.map(_.port.decl).distinct
      acceptsPorts.map { port => Accepts(glue.accepts.filter(port == _.port.decl), ports) }
    }

    val constraints = (portEnablesInputsOutputs
      ++ outputDataNeedsPort
      ++ inputDataNeedsPort
      ++ inputRequiresOneActiveWire
      ++ requires
      ++ accepts
      ++ wireRequiresPorts
      :+ ExcludeEmptySynchronization(ports)
      )

    Using(SolverContextFactory.createSolverContext(Solvers.SMTINTERPOL)) { ctx =>
      Using(ctx.newProverEnvironment(ProverOptions.GENERATE_MODELS, ProverOptions.GENERATE_ALL_SAT)) { prover =>
        val bipSmt = BipSmt(ctx)

        val formulas = constraints.map(_.toBooleanFormula(bipSmt))
        val combinedFormula = formulas.fold(bipSmt.bm.makeTrue()) { (l, r) => bipSmt.bm.and(l, r) }
        val allVars = bipSmt.fm.extractVariables(combinedFormula).keySet().asScala.toSeq.sorted.map(bipSmt.bm.makeVariable)
        logger.debug("=== Formula: all BIP constraints ===")
        logger.debug(new PrettyPrinter(bipSmt.fm).formulaToString(combinedFormula))
        logger.debug("=== End of formula: all BIP constraints ===")
        formulas.foreach(prover.addConstraint)

        val synchronizations: mutable.ArrayBuffer[BipPortSynchronization[Pre]] = mutable.ArrayBuffer()
        while (!prover.isUnsat) {
          val (synchronization, modelFormula) = Using(prover.getModel()) { model =>
            val enabledPorts = ports.filter { port => model.evaluate(bipSmt.get(port)) }.map(_.ref)
            val enabledWires = wires.filter { wire => model.evaluate(bipSmt.get(wire)) }

            val modelFormula: BooleanFormula = bipSmt.bm.and(
              allVars.map { v => bipSmt.bm.equivalence(v, bipSmt.bm.makeBoolean(model.evaluate(v))) } : _*)

            (new BipPortSynchronization[Pre](enabledPorts, enabledWires), modelFormula)
          }.get

          logger.debug(synchronization.summarize)
          synchronizations.addOne(synchronization)
          prover.addConstraint(bipSmt.bm.not(modelFormula))
        }

        logger.debug(s"Possible synchronizations found: ${synchronizations.size}")

        val portInt = ports.map { p => p -> ports.indexOf(p) }.toMap
        val wireInt = wires.map { w => w -> wires.indexOf(w) }.toMap
        import scala.math.Ordering.Implicits._ // Required for ordering of Seq[Int]
        synchronizations.sortBy { synchronization =>
          (synchronization.ports.map(p => portInt(p.decl)), synchronization.wires.map(wireInt(_)))
        }.toSeq
      }
    }.get.get
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case glue: BipGlue[Pre] =>
      computeGlue(glue).foreach(dispatch)
    case other => rewriteDefault(other)
  }
}
