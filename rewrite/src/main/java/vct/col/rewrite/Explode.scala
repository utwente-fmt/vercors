package vct.col.rewrite

import vct.col.ast._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{AbstractApplicable, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.Explode.{ExplodeOrigin, UnknownDeclaration}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.SystemError

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case object Explode extends RewriterBuilder {
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
}

case class Explode[Pre <: Generation]() extends Rewriter[Pre] {
  case class SplitProgram(
    adts: Seq[AxiomaticDataType[Pre]],
    fields: Seq[SilverField[Pre]],
    funcs: Seq[Function[Pre]],
    preds: Seq[Predicate[Pre]],
    procs: Seq[Procedure[Pre]],
  ) {
    @tailrec
    private def fixpoint[G, T <: Node[G]](decls: Seq[T], scan: Seq[Node[G]], usages: Node[G] => Seq[T], prevUsages: Set[T] = Set.empty[T]): Seq[T] = {
      val curUsages = (scan ++ prevUsages).flatMap(usages).toSet
      if(curUsages == prevUsages) decls.filter(curUsages.contains)
      else fixpoint(decls, scan, usages, curUsages)
    }

    def focus(proc: Procedure[Pre]): Seq[GlobalDeclaration[Post]] = {
      val invocations = proc.collect { case inv: InvokeProcedure[Pre] => inv.ref.decl }.toSet

      val focusProcs: Seq[Procedure[Pre]] = procs.filter(p => invocations.contains(p) && p != proc)
      val scanProcs: Seq[Node[Pre]] = proc +: focusProcs.flatMap(p => p.contract +: p.returnType +: (p.args ++ p.outArgs ++ p.typeArgs))

      val focusPreds: Seq[Predicate[Pre]] = fixpoint[Pre, Predicate[Pre]](preds, scanProcs, (n: Node[Pre]) => n.collect {
        case PredicateApply(Ref(p), _, _) => p
        case PredicateLocation(Ref(p), _) => p
      })
      val scanPreds: Seq[Node[Pre]] = scanProcs ++ focusPreds

      val naiveFocusFuncs: Seq[Function[Pre]] = fixpoint[Pre, Function[Pre]](funcs, scanPreds, (n: Node[Pre]) => n.collect {
        case FunctionInvocation(Ref(f), _, _, _, _) => f
      })
      val focusFuncs = funcs.filter(f => f.contract.decreases.isEmpty || naiveFocusFuncs.contains(f))
      val scanFuncs: Seq[Node[Pre]] = scanPreds ++ focusFuncs

      val focusFields: Seq[SilverField[Pre]] = fixpoint[Pre, SilverField[Pre]](fields, scanFuncs, (n: Node[Pre]) => n.collect {
        case SilverDeref(_, Ref(f)) => f
        case SilverFieldLocation(_, Ref(f)) => f
      })

      implicit val o: Origin = ExplodeOrigin

      globalDeclarations.collect {
        adts.foreach(dispatch)
        focusFields.foreach(dispatch)
        focusFuncs.foreach(f => globalDeclarations.succeed(f, f.rewrite(
          blame = AbstractApplicable,
          body = None,
          contract = f.contract.rewrite(
            ensures = if(f.body.isEmpty) dispatch(f.contract.ensures) else SplitAccountedPredicate(dispatch(f.contract.ensures), UnitAccountedPredicate(
              Result[Post](succ(f)) === dispatch(f.body.get)
            )),
          ),
        )))
        focusPreds.foreach(dispatch)
        dispatch(proc)
        focusProcs.foreach(p => globalDeclarations.succeed(p, p.rewrite(body = None)))
      }._1
    }
  }

  def split(program: Program[Pre]): SplitProgram = {
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

    SplitProgram(adts.toSeq, fields.toSeq, funcs.toSeq, preds.toSeq, procs.toSeq)
  }

  override def dispatch(verification: Verification[Pre]): Verification[Post] =
    verification.rewrite(tasks = verification.tasks.flatMap(explode))

  def make(context: VerificationContext[Pre], decls: => Seq[GlobalDeclaration[Post]]): VerificationContext[Post] =
    VerificationContext(context.program.rewrite(declarations = decls))(context.o)

  def explode(context: VerificationContext[Pre]): Seq[VerificationContext[Post]] = {
    val program = split(context.program)

    make(context, globalDeclarations.dispatch(program.adts ++ program.fields ++ program.funcs ++ program.preds)) +:
      program.procs.map(proc => make(context, program.focus(proc)))
  }
}
