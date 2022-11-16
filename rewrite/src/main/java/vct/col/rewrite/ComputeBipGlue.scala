package vct.col.rewrite

import com.typesafe.scalalogging.LazyLogging
import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.{BasicLogManager, LogManager}
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import org.sosy_lab.java_smt.api.SolverContext.ProverOptions
import vct.col.ast.{BipData, BipGlue, BipGlueAccepts, BipGlueDataWire, BipGlueRequires, BipIncomingData, BipOutgoingData, BipPort, BipSynchronization, BipTransition, Class, ClassDeclaration, Declaration, Node, Program}
import vct.col.ref.Ref
import org.sosy_lab.java_smt.api.{BooleanFormula, BooleanFormulaManager, SolverContext}
import org.sosy_lab.java_smt.utils.PrettyPrinter

import scala.collection.immutable.{AbstractSeq, LinearSeq}
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

    def mark(name: String): BooleanFormula = {
      val nameUnique = uniqueName(name)
      names.add(nameUnique)
      bm.makeVariable(nameUnique)
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
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula = {
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
  case class WireRequiresPorts(wire: BipGlueDataWire[Pre]) extends Constraint {
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula =
      ctx.bm.implication(
        ctx.get(wire),
        ctx.bm.and(
          ctx.get(wire.dataOut.decl),
          ctx.get(wire.dataIn.decl)
        )
      )

  }
  case class ExcludeSynchronization(synchronization: BipSynchronization[Pre]) extends Constraint {
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula =
      ctx.bm.not(ctx.bm.and(
        synchronization.ports.map { p => ctx.get(p.decl) } ++ synchronization.wires.map(ctx.get) : _*
      ))
  }
  case class Requires(requires: Seq[BipGlueRequires[Pre]]) extends Constraint {
    // There should be > 0 clauses, and all clauses should concern the same port
    assert(requires.forall(requires.head.port.decl == _.port.decl))

    val port = requires.head.port.decl
    lazy val others = requires.flatMap(_.others).map(_.decl)
    override def toBooleanFormula(ctx: BipSmt): BooleanFormula =
      ctx.bm.implication(
        ctx.get(port),
        ctx.bm.or(others.map(ctx.get): _*)
      )
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

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case glue: BipGlue[Pre] =>
      val relevantDecls: Set[ClassDeclaration[Pre]] = glue.transSubnodes.collect {
        case BipGlueRequires(Ref(port), requires) =>
          port +: requires.map(_.decl)
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
        // There might actually be multiple transitions given a port. However, these should be guaranteed to all have the same input datas.
        // This is because you can put multiple @Transition annotations on a method given a port, but not @Transition annotations for one
        // specific port on many different methods. (According to with Larisa)
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
        transitions.flatMap { transition =>
          transition.data.map { data => (data.decl, transition.port.decl) }
        }.groupMap(_._1)(_._2).map { case (data, ports) => InputDataNeedsPort(data, ports) }
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
      val wireRequiresPorts: Seq[Constraint] = wires.map(WireRequiresPorts)

      val requires = glue.requires.groupBy(_.port.decl).values.map(Requires)
      val accepts = glue.accepts.groupBy(_.port.decl).values.map(Accepts(_, ports))
      val excludeEmptySynchronization = ExcludeEmptySynchronization(ports)

      val constraints = (portEnablesInputsOutputs
        ++ outputDataNeedsPort
        ++ inputDataNeedsPort
        ++ inputRequiresOneActiveWire
        ++ requires
        ++ accepts
        ++ wireRequiresPorts
        :+ excludeEmptySynchronization // <--
        )

      Using(SolverContextFactory.createSolverContext(Solvers.SMTINTERPOL)) { ctx =>
        Using(ctx.newProverEnvironment(ProverOptions.GENERATE_MODELS)) { prover =>
          val bipSmt = BipSmt(ctx)

          val bs = constraints.map { c =>
            val f = c.toBooleanFormula(bipSmt)
            // TODO: Make logging proper
//            logger.info(new PrettyPrinter(bipSmt.fm).formulaToString(f, PrettyPrinter.PrinterOption.SPLIT_ONLY_BOOLEAN_OPERATIONS))
            f
          }
          val all = bs.fold(bipSmt.bm.makeTrue()) { (l, r) => bipSmt.bm.and(l, r) }
          // TODO: Make logging proper
//          logger.info("== Pretty printer ==")
//          logger.info(new PrettyPrinter(bipSmt.fm).formulaToString(all, PrettyPrinter.PrinterOption.SPLIT_ONLY_BOOLEAN_OPERATIONS))
          bs.foreach(prover.addConstraint)
          // TODO: consider allSat?
          // TODO: In synchrons, data wires that are not needed should be disabled. Also: if a transition has no input data, the local datas should be disabled.
          val models: mutable.ArrayBuffer[BipSynchronization[Pre]] = mutable.ArrayBuffer()
          while (!prover.isUnsat) {
            logger.info("Sat!")
            val synchronization = Using(prover.getModel()) { model =>
              val enabledPorts = ports.filter { port => model.evaluate(bipSmt.get(port)) }.map(_.ref)
              val enabledWires = wires.filter { wire => model.evaluate(bipSmt.get(wire)) }
              new BipSynchronization[Pre](enabledPorts, enabledWires)
            }.get

            def txt(s: BipSynchronization[Pre]): String = {
              val portsTxt = if(s.ports.isEmpty) "No ports" else s.ports.map("- " + _.decl.o.preferredName).mkString("\n")
              val wiresTxt = if(s.wires.isEmpty) "No wires" else s.wires.map("- " + _.o.preferredName).mkString("\n")

              s"""=== Synchronization ===
                 |Ports:
                 |${portsTxt}
                 |Wires:
                 |${wiresTxt}""".stripMargin
            }

            logger.info(txt(synchronization))
            models.addOne(synchronization)
            prover.addConstraint(ExcludeSynchronization(synchronization).toBooleanFormula(bipSmt))
          }

          logger.info("Unsat!")
          logger.info(s"Possible synchronizations found: ${models.size}")

          val core = prover.getUnsatCore.asScala
          logger.info(core.size.toString)
          val xx = core.fold(bipSmt.bm.makeTrue()) { (l, r) => bipSmt.bm.and(l, r) }
          logger.info("==== " + bipSmt.fm.dumpFormula(xx).toString)
//          core.foreach { b =>
//            logger.info(bipSmt.fm.dumpFormula(b).toString)
//          }

          System.exit(0)
        }
      }
      System.exit(0)

    case other => rewriteDefault(other)
  }
}
