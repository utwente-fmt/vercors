package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.util.AstBuildHelpers._
import vct.col.ast.{Applicable, ApplicableContract, ArraySubscript, BooleanValue, Class, ContractApplicable, Declaration, Deref, Endpoint, EndpointUse, EnumUse, Expr, FieldLocation, Function, InstanceField, InstanceFunction, IterationContract, Length, Local, LoopContract, LoopInvariant, Null, Perm, Procedure, Result, SeqProg, SeqRun, SplitAccountedPredicate, TArray, TClass, TInt, ThisObject, Type, UnitAccountedPredicate, Variable, WritePerm}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg}

object GenerateSeqProgPermissions extends RewriterBuilderArg[Boolean] {
  override def key: String = "generateSeqProgPermissions"
  override def desc: String = "Generates permissions for fields of some types (classes, int, bool, and arrays of these) for constructs used inside seq_program."
}

case class GenerateSeqProgPermissions[Pre <: Generation](enabled: Boolean = false) extends Rewriter[Pre] with LazyLogging {

//  sealed trait Context
//  case class ClassContext(cls: Class[Pre]) extends Context
//  case class SeqContext(prog: SeqProg[Pre]) extends Context

  val currentPerm: ScopedStack[Expr[Post]] = ScopedStack()

  /* - Permission generation table -
      Only considered nodes so far that occur in early VeyMont case studies.

                                  Pre             Post                    Invariant
                       Function:  args                                    N.a
                      Procedure:  args            args, return            args

              Instance function:  args, fields                            N.a
                Instance method:  args, fields    args, fields, return    args, fields

                        SeqProg:  args            args                    N.a
                         SeqRun:  endpoints       endpoints               endpoints
   */

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
//    case prog: SeqProg[Pre] if enabled => currentContext.having(SeqContext(prog)) { rewriteDefault(prog) }
    case fun: Function[Pre] if enabled =>
      globalDeclarations.declare(fun.rewrite(
        contract = prependContract(fun.contract, variablesPerm(fun.args)(fun.o), tt)(fun.o)
      ))
    case proc: Procedure[Pre] if enabled =>
      implicit val o = proc.o
      globalDeclarations.declare(proc.rewrite(
        contract = prependContract(
          proc.contract,
          variablesPerm(proc.args),
          variablesPerm(proc.args) &* resultPerm(proc)(proc.o)),
        body = proc.body.map(body => currentPerm.having(variablesPerm(proc.args)) { dispatch(body) })
      ))

    case cls: Class[Pre] if enabled => currentPerm.having(???) { rewriteDefault(???) }

    case fun: InstanceFunction[Pre] if enabled =>

    case decl => rewriteDefault(decl)
  }

  def prependContract(contract: ApplicableContract[Pre], pre: Expr[Post], post: Expr[Post])(implicit o: Origin) =
    contract.rewrite(
      requires = pre match {
        case BooleanValue(true) => dispatch(contract.requires)
        case pre => SplitAccountedPredicate[Post](UnitAccountedPredicate(pre), dispatch(contract.requires))
      },
      ensures = post match {
        case BooleanValue(true) => dispatch(contract.ensures)
        case post => SplitAccountedPredicate[Post](UnitAccountedPredicate(post), dispatch(contract.ensures))
      }
    )

  override def dispatch(loopContract: LoopContract[Pre]): LoopContract[Post] =
    (currentPerm.topOption, loopContract) match {
      case (Some(perm), invariant: LoopInvariant[pre]) =>
        implicit val o = loopContract.o
        invariant.rewrite(invariant = perm &* dispatch(invariant.invariant))
      case (Some(perm), iteration: IterationContract[pre]) =>
        implicit val o = loopContract.o
        iteration.rewrite(
          requires = perm &* dispatch(iteration.requires),
          ensures = perm &* dispatch(iteration.ensures))
      case _ => rewriteDefault(loopContract)
    }

//  def permissions(ctx: Context)(implicit o: Origin): Expr[Post] = ctx match {
//    case SeqContext(prog) => foldStar[Post](prog.endpoints.map(endpointPerm))
//    case ClassContext(cls) => transitivePerm(ThisObject[Post](succ(cls)), TClass(cls.ref))
//  }

  def endpointPerm(endpoint: Endpoint[Pre])(implicit o: Origin): Expr[Post] =
    transitivePerm(EndpointUse[Post](succ(endpoint)), TClass(endpoint.cls))

  def variablePerm(variable: Variable[Pre]): Expr[Post] =
    transitivePerm(Local[Post](succ(variable))(variable.o), variable.t)(variable.o)

  def variablesPerm(variables: Seq[Variable[Pre]])(implicit o: Origin): Expr[Post] =
    foldStar(variables.map(variablePerm))

  def resultPerm(app: ContractApplicable[Pre])(implicit o: Origin): Expr[Post] =
    transitivePerm(Result[Post](anySucc(app)), app.returnType)

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
