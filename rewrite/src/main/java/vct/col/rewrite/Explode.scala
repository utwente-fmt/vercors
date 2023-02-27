package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.{AbstractApplicable, Blame, Origin, PanicBlame, UnsafeDontCare, VerificationFailure}
import vct.col.ref.Ref
import vct.col.rewrite.Explode.{AssumeFunction, UnknownDeclaration, VerifiedElsewhereBlame}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.SystemError

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case object Explode extends RewriterBuilderArg[Boolean] {
  override def key: String = "explode"
  override def desc: String = "Split out verifications over entities, by eliding unrelated declarations and abstracting relevant ones."

  case class UnknownDeclaration(decl: Declaration[_]) extends SystemError {
    override def text: String = s"Unknown declaration kind at this point: ${decl.getClass.getSimpleName}"
  }

  case object ExplodeOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def context: String = "At: [node generated to split out verification]"
    override def inlineContext: String = "[Node generated to split out verification]"
    override def shortPosition: String = "generated"
  }

  case object VerifiedElsewhereBlame extends Blame[VerificationFailure] {
    override def blame(error: VerificationFailure): Unit = {
      // Do nothing: the error will already be reported elsewhere (or at worst nont-deterministically verified)
    }
  }

  case class AssumeFunction(inner: Origin) extends Origin {
    override def preferredName: String = "assume" + inner.preferredName.capitalize
    override def context: String = inner.context
    override def inlineContext: String = inner.inlineContext
    override def shortPosition: String = inner.shortPosition
  }
}

case class Explode[Pre <: Generation](enable: Boolean) extends Rewriter[Pre] {
  case class FocusedProgram(
    adts: Seq[AxiomaticDataType[Pre]],
    fields: Seq[SilverField[Pre]],
    funcs: Seq[Function[Pre]],
    predOutlines: Seq[Predicate[Pre]],
    predBodies: Seq[Predicate[Pre]],
    procOutlines: Seq[Procedure[Pre]],
    procBodies: Seq[Procedure[Pre]],
  ) {
    require(procBodies.forall(procOutlines.contains))
    require(predBodies.forall(predOutlines.contains))

    lazy val scanNodes: Seq[Node[Pre]] =
      adts ++
        fields ++
        funcs ++
        // PB: it's probably nonsense to include args, types, but for consistency: all subnodes except the body
        predOutlines.flatMap(_.args) ++
        predBodies.flatMap(_.body.toSeq) ++
        procOutlines.flatMap(p => p.contract +: p.returnType +: (p.args ++ p.outArgs ++ p.typeArgs)) ++
        procBodies.flatMap(_.body.toSeq)

    def fieldUsage: Seq[SilverField[Pre]] =
      scanNodes.flatMap(_.flatCollect {
        case SilverDeref(_, Ref(f)) => Seq(f)
        case SilverFieldLocation(_, Ref(f)) => Seq(f)
        case SilverCurFieldPerm(_, Ref(f)) => Seq(f)
        case SilverNewRef(_, fs) => fs.map(_.decl)
        case SilverFieldAssign(_, Ref(f), _) => Seq(f)
      }).distinct

    def funcUsage: Seq[Function[Pre]] =
      scanNodes.flatMap(_.collect {
        case FunctionInvocation(Ref(f), _, _, _, _) => f
      }).distinct

    def predUsage: Seq[Predicate[Pre]] =
      scanNodes.flatMap(_.collect {
        case PredicateApply(Ref(p), _, _) => p
        case PredicateLocation(Ref(p), _) => p
        case SilverCurPredPerm(Ref(p), _) => p
      }).distinct

    def predContentUsage: Seq[Predicate[Pre]] =
      scanNodes.flatMap(_.collect {
        case Fold(PredicateApply(Ref(p), _, _)) => p
        case Unfold(PredicateApply(Ref(p), _, _)) => p
        case Unfolding(PredicateApply(Ref(p), _, _), _) => p
      }).distinct

    def procUsage: Seq[Procedure[Pre]] =
      scanNodes.flatMap(_.collect {
        case InvokeProcedure(Ref(p), _, _, _, _, _) => p
        case ProcedureInvocation(Ref(p), _, _, _, _, _) => p
      })

    def step: FocusedProgram =
      FocusedProgram(
        adts,
        (fields ++ fieldUsage).distinct,
        (funcs ++ funcUsage).distinct,
        (predOutlines ++ predUsage).distinct,
        (predBodies ++ predContentUsage).distinct,
        (procOutlines ++ procUsage).distinct,
        procBodies,
      )

    @tailrec
    final def fixpoint: FocusedProgram = {
      val next = step
      if(this == next) this
      else next.fixpoint
    }

    def sort(other: FocusedProgram): FocusedProgram =
      FocusedProgram(
        adts.filter(other.adts.contains),
        fields.filter(other.fields.contains),
        funcs.filter(other.funcs.contains),
        predOutlines.filter(other.predOutlines.contains),
        predBodies.filter(other.predBodies.contains),
        procOutlines.filter(other.procOutlines.contains),
        procBodies.filter(other.procBodies.contains),
      )

    def toDecls: Seq[GlobalDeclaration[Post]] =
      globalDeclarations.collect {
        verifiedElsewhere.having(()) {
          adts.foreach(dispatch)
          fields.foreach(dispatch)
          funcs.foreach(dispatch)
          predOutlines.foreach { pred =>
            if(predBodies.contains(pred)) dispatch(pred)
            else globalDeclarations.succeed(pred, pred.rewrite(body = None))
          }
          procOutlines.filterNot(procBodies.contains).foreach { proc =>
            globalDeclarations.succeed(proc, proc.rewrite(body = None))
          }
        }
        procBodies.foreach(dispatch)
      }._1

    def focus(proc: Procedure[Pre]): Seq[GlobalDeclaration[Post]] =
      sort(FocusedProgram(adts, Nil, funcs.filter(_.contract.decreases.isEmpty), Nil, Nil, Seq(proc), Seq(proc)).fixpoint).toDecls
  }

  val verifiedElsewhere: ScopedStack[Unit] = ScopedStack()

  override def dispatch[T <: VerificationFailure](blame: Blame[T]): Blame[T] =
    if(verifiedElsewhere.nonEmpty) VerifiedElsewhereBlame else blame

  def split(program: Program[Pre]): FocusedProgram = {
    val adts: ArrayBuffer[AxiomaticDataType[Pre]] = ArrayBuffer()
    val fields: ArrayBuffer[SilverField[Pre]] = ArrayBuffer()
    val funcs: ArrayBuffer[Function[Pre]] = ArrayBuffer()
    val preds: ArrayBuffer[Predicate[Pre]] = ArrayBuffer()
    val procs: ArrayBuffer[Procedure[Pre]] = ArrayBuffer()

    program.declarations.foreach {
      case adt: AxiomaticDataType[Pre] => adts += adt
      case field: SilverField[Pre] => fields += field
      case func: Function[Pre] => funcs += func
      case pred: Predicate[Pre] => preds += pred
      case proc: Procedure[Pre] => procs += proc
      case other => throw UnknownDeclaration(other)
    }

    FocusedProgram(adts.toSeq, fields.toSeq, funcs.toSeq, preds.toSeq, preds.toSeq, procs.toSeq, procs.toSeq)
  }

  override def dispatch(verification: Verification[Pre]): Verification[Post] =
    if(enable) verification.rewrite(tasks = verification.tasks.flatMap(explode))
    else verification.rewrite()

  def make(context: VerificationContext[Pre], decls: => Seq[GlobalDeclaration[Post]]): VerificationContext[Post] =
    VerificationContext(context.program.rewrite(declarations = decls))(context.o)

  def explode(context: VerificationContext[Pre]): Seq[VerificationContext[Post]] = {
    val program = split(context.program)

    make(context, globalDeclarations.dispatch(program.adts ++ program.fields ++ program.funcs ++ program.predBodies)) +:
      program.procBodies.map(proc => make(context, program.focus(proc)))
  }
}
