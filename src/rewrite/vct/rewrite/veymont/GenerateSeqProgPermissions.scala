package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.util.AstBuildHelpers._
import vct.col.ast.{ApplicableContract, ArraySubscript, Declaration, Deref, Endpoint, EndpointUse, EnumUse, Expr, FieldLocation, InstanceField, IterationContract, Length, Local, LoopContract, LoopInvariant, Null, Perm, SeqProg, SeqRun, SplitAccountedPredicate, TArray, TClass, TInt, Type, UnitAccountedPredicate, WritePerm}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg}

object GenerateSeqProgPermissions extends RewriterBuilderArg[Boolean] {
  override def key: String = "generateSeqProgPermissions"
  override def desc: String = "Generates permissions for fields of some types (classes, int, bool, and arrays of these) for constructs used inside seq_program."
}

case class GenerateSeqProgPermissions[Pre <: Generation](enabled: Boolean = false) extends Rewriter[Pre] with LazyLogging {

  val currentProg: ScopedStack[SeqProg[Pre]] = ScopedStack()
  val currentRun: ScopedStack[SeqRun[Pre]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] => currentProg.having(prog) { rewriteDefault(prog) }
    case decl => rewriteDefault(decl)
  }

  override def dispatch(run: SeqRun[Pre]): SeqRun[Post] = currentRun.having(run) { rewriteDefault(run) }

  override def dispatch(contract: ApplicableContract[Pre]): ApplicableContract[Post] =
    (currentProg.topOption, currentRun.topOption) match {
      case (Some(prog), Some(_)) if enabled =>
        implicit val o = contract.o
        contract.rewrite(
          requires =
            SplitAccountedPredicate[Post](
              UnitAccountedPredicate(foldStar[Post](prog.endpoints.map(endpointPerm))),
              dispatch(contract.requires)
            ),
          ensures =
           SplitAccountedPredicate[Post](
             UnitAccountedPredicate(foldStar[Post](prog.endpoints.map(endpointPerm))),
             dispatch(contract.ensures)
           )
        )
      case _ => rewriteDefault(contract)
    }

  override def dispatch(loopContract: LoopContract[Pre]): LoopContract[Post] = (currentProg.topOption, currentRun.topOption, loopContract) match {
    case (Some(prog), Some(_), invariant: LoopInvariant[pre]) if enabled =>
      implicit val o = loopContract.o
      invariant.rewrite(
        invariant = foldStar[Post](prog.endpoints.map(endpointPerm) :+ dispatch(invariant.invariant))
      )
    case (Some(prog), Some(_), iteration: IterationContract[pre]) if enabled =>
      implicit val o = loopContract.o
      iteration.rewrite(
        requires = foldStar[Post](prog.endpoints.map(endpointPerm) :+ dispatch(iteration.requires)),
        ensures = foldStar[Post](prog.endpoints.map(endpointPerm) :+ dispatch(iteration.ensures))
      )
    case _ => rewriteDefault(loopContract)
  }

  def endpointPerm(endpoint: Endpoint[Pre])(implicit o: Origin): Expr[Post] =
    transitivePerm(EndpointUse[Post](succ(endpoint)), TClass(endpoint.cls))

  /*

  int x;
  => perm(x, 1);

  int[] x;
  => perm(x, 1) ** x != null ** (\forall int i = 0 .. x.length; perm(x[i], 1))

  cell x;
  => perm(x, 1) ** perm(x.v, 1)

  cell[] x;
  => perm(x, 1) ** x != null ** (\forall int i = 0 .. x.length; perm(x[i], 1) ** perm(x[i].v, 1))

   */

  def transitivePerm(e: Expr[Post], t: Type[Pre])(implicit o: Origin): Expr[Post] = t match {
    case TArray(u) =>
      (e !== Null()) &*
      starall[Post](
        /* Only with --assumeinjectivityoninhale. However, this will be deleted in later versions of veymont,
                 so it's fine if its incomplete */
        PanicBlame("Quantifying over an array should be injective."),
        TInt(),
        (i: Local[Post]) => ((const[Post](0) <= i) && (i < Length(e)(PanicBlame("Array is guaranteed non-null")))) ==>
          (arrayPerm(e, i, WritePerm(), PanicBlame("Encoding guarantees well-formedness")) &*
            transitivePerm(ArraySubscript(e, i)(PanicBlame("Encoding guarantees well-formedness")), u))
      )
    case TClass(Ref(cls)) => foldStar(cls.collect { case f: InstanceField[Pre] => fieldTransitivePerm(e, f) })
    case _ => tt
  }

  def fieldTransitivePerm(`this`: Expr[Post], f: InstanceField[Pre])(implicit o: Origin): Expr[Post] = {
    fieldPerm[Post](`this`, succ(f), WritePerm()) &*
      transitivePerm(Deref[Post](`this`, succ(f))(PanicBlame("Permission for this field is already established")), f.t)
  }

}
