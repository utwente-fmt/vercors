package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.util.AstBuildHelpers._
import vct.col.ast.{ApplicableContract, ArraySubscript, Class, Declaration, Deref, Endpoint, EndpointUse, EnumUse, Expr, FieldLocation, InstanceField, IterationContract, Length, Local, LoopContract, LoopInvariant, Null, Perm, SeqProg, SeqRun, SplitAccountedPredicate, TArray, TClass, TInt, ThisObject, Type, UnitAccountedPredicate, WritePerm}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg}

object GenerateSeqProgPermissions extends RewriterBuilderArg[Boolean] {
  override def key: String = "generateSeqProgPermissions"
  override def desc: String = "Generates permissions for fields of some types (classes, int, bool, and arrays of these) for constructs used inside seq_program."
}

case class GenerateSeqProgPermissions[Pre <: Generation](enabled: Boolean = false) extends Rewriter[Pre] with LazyLogging {

  sealed trait Context
  case class ClassContext(cls: Class[Pre]) extends Context
  case class SeqContext(prog: SeqProg[Pre]) extends Context

  val currentContext: ScopedStack[Context] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case prog: SeqProg[Pre] if enabled => currentContext.having(SeqContext(prog)) { rewriteDefault(prog) }
    case cls: Class[Pre] if enabled => currentContext.having(ClassContext(cls)) { rewriteDefault(cls) }
    case decl => rewriteDefault(decl)
  }

  override def dispatch(contract: ApplicableContract[Pre]): ApplicableContract[Post] = currentContext.topOption match {
    case Some(ctx) =>
      implicit val o = contract.o
      contract.rewrite(
        requires =
          SplitAccountedPredicate[Post](
            UnitAccountedPredicate(permissions(ctx)),
            dispatch(contract.requires)
          ),
        ensures =
         SplitAccountedPredicate[Post](
           UnitAccountedPredicate(permissions(ctx)),
           dispatch(contract.ensures)
         )
      )
    case _ => rewriteDefault(contract)
  }

  override def dispatch(loopContract: LoopContract[Pre]): LoopContract[Post] = (currentContext.topOption, loopContract) match {
    case (Some(ctx), invariant: LoopInvariant[pre]) =>
      implicit val o = loopContract.o
      invariant.rewrite(
        invariant = foldStar[Post](Seq(permissions(ctx), dispatch(invariant.invariant)))
      )
    case (Some(ctx), iteration: IterationContract[pre]) =>
      implicit val o = loopContract.o
      iteration.rewrite(
        requires = foldStar[Post](Seq(permissions(ctx), dispatch(iteration.requires))),
        ensures = foldStar[Post](Seq(permissions(ctx), dispatch(iteration.ensures)))
      )
    case _ => rewriteDefault(loopContract)
  }

  def permissions(ctx: Context)(implicit o: Origin): Expr[Post] = ctx match {
    case SeqContext(prog) => foldStar[Post](prog.endpoints.map(endpointPerm))
    case ClassContext(cls) => transitivePerm(ThisObject[Post](succ(cls)), TClass(cls.ref))
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
