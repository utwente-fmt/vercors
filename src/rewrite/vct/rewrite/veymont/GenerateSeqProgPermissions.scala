package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.util.AstBuildHelpers._
import vct.col.ast.{Applicable, ApplicableContract, ArrayLocation, ArraySubscript, BooleanValue, Class, ContractApplicable, Declaration, Deref, Endpoint, EndpointGuard, EndpointName, EndpointUse, EnumUse, Expr, FieldLocation, Function, InlinePattern, InstanceField, InstanceFunction, InstanceMethod, IterationContract, Length, Local, LoopContract, LoopInvariant, Node, Null, Perm, Procedure, Result, SeqAssign, SeqLoop, SeqProg, SeqRun, SplitAccountedPredicate, Statement, TArray, TClass, TInt, ThisObject, Type, UnitAccountedPredicate, Variable, WritePerm}
import vct.col.ast.declaration.global.SeqProgImpl.participants
import vct.col.origin.{Origin, PanicBlame}
import vct.col.ref.Ref
import vct.col.resolve.ctx.Referrable
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}

import scala.collection.immutable.ListSet

object GenerateSeqProgPermissions extends RewriterBuilderArg[Boolean] {
  override def key: String = "generateSeqProgPermissions"
  override def desc: String = "Generates permissions for fields of some types (classes, int, bool, and arrays of these) for constructs used inside seq_program."
}

case class GenerateSeqProgPermissions[Pre <: Generation](enabled: Boolean = false) extends Rewriter[Pre] with LazyLogging {

  val currentPerm: ScopedStack[Expr[Post]] = ScopedStack()
  val currentProg: ScopedStack[SeqProg[Pre]] = ScopedStack()
  val generatingClasses: ScopedStack[Class[Pre]] = ScopedStack()
  val generatingOrigin: ScopedStack[Node[Pre]] = ScopedStack()

  /* - Permission generation table -
      Only considers nodes as necessary for VeyMont case studies.

                                  Pre             Post                    Invariant
                       Function:  args                                    N.a
                      Procedure:  args            args, return            args

              Instance function:  args, fields                            N.a
                Instance method:  args, fields    args, fields, return    args, fields

                        SeqProg:  args            args                    N.a
                         SeqRun:  endpoints       endpoints               endpoints
   */

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case fun: Function[Pre] if enabled =>
      globalDeclarations.succeed(fun, fun.rewrite(
        contract = prependContract(fun.contract, variablesPerm(fun.args)(fun.o), tt)(fun.o)
      ))
    case proc: Procedure[Pre] if enabled =>
      implicit val o = proc.o
      globalDeclarations.succeed(proc, proc.rewrite(
        contract = prependContract(
          proc.contract,
          variablesPerm(proc.args),
          variablesPerm(proc.args) &* resultPerm(proc)(proc.o)),
        body = proc.body.map(body => currentPerm.having(variablesPerm(proc.args)) { dispatch(body) })
      ))

    case cls: Class[Pre] if enabled => currentPerm.having(classPerm(cls)) { rewriteDefault(cls) }

    case fun: InstanceFunction[Pre] if enabled =>
      implicit val o = fun.o
      classDeclarations.succeed(fun, fun.rewrite(
        contract = prependContract(
          fun.contract,
          currentPerm.top &* variablesPerm(fun.args),
          tt)))

    case method: InstanceMethod[Pre] if enabled && currentProg.nonEmpty =>
      implicit val o = method.o
      classDeclarations.succeed(method, method.rewrite(
        contract = prependContract(
          method.contract,
          endpointsPerm(participants(method).toSeq),
          endpointsPerm(participants(method).toSeq)
        )
      ))

    case method: InstanceMethod[Pre] if enabled =>
      // Permission generation for InstanceMethods in classes
      implicit val o = method.o
      classDeclarations.succeed(method, method.rewrite(
        contract = prependContract(
          method.contract,
          currentPerm.top &* variablesPerm(method.args),
          if(!method.pure) currentPerm.top &* resultPerm(method) else tt
        )
      ))

    case prog: SeqProg[Pre] if enabled =>
      val run = prog.run
      currentProg.having(prog) {
        globalDeclarations.succeed(prog, prog.rewrite(
          contract = prependContract(
            prog.contract,
            variablesPerm(prog.args)(prog.o),
            variablesPerm(prog.args)(prog.o)
          )(prog.o),
          run = run.rewrite(
            contract = prependContract(
              run.contract,
              endpointsPerm(prog.endpoints)(run.o),
              endpointsPerm(prog.endpoints)(run.o),
            )(run.o),
            body = currentPerm.having(endpointsPerm(prog.endpoints)(run.o)) {
              rewriteDefault(run.body)
            })))
      }

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

  override def dispatch(statement: Statement[Pre]): Statement[Post] = statement match {
    case loop: SeqLoop[Pre] =>
      currentPerm.having(endpointsPerm(participants(statement).toSeq)(loop.contract.o)) {
        loop.rewriteDefault()
      }
    case statement => rewriteDefault(statement)
  }

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

  def endpointPerm(endpoint: Endpoint[Pre])(implicit o: Origin): Expr[Post] =
    transitivePerm(EndpointUse[Post](succ(endpoint)), TClass(endpoint.cls))

  def endpointsPerm(endpoints: Seq[Endpoint[Pre]])(implicit o: Origin): Expr[Post] =
    foldStar(endpoints.map(endpointPerm))

  def variablePerm(variable: Variable[Pre]): Expr[Post] =
    transitivePerm(Local[Post](succ(variable))(variable.o), variable.t)(variable.o)

  def variablesPerm(variables: Seq[Variable[Pre]])(implicit o: Origin): Expr[Post] =
    foldStar(variables.map(variablePerm))

  def resultPerm(app: ContractApplicable[Pre])(implicit o: Origin): Expr[Post] =
    transitivePerm(Result[Post](anySucc(app)), app.returnType)

  def classPerm(cls: Class[Pre]): Expr[Post] =
    transitivePerm(ThisObject[Post](succ(cls))(cls.o), TClass(cls.ref))(cls.o)

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
//          (arrayPerm(e, i, WritePerm(), PanicBlame("Encoding guarantees well-formedness")) &*
            (Perm(ArrayLocation(e, i)(PanicBlame("Encoding guarantees well-formedness")), WritePerm()) &*
              transitivePerm(ArraySubscript(e, i)(PanicBlame("Encoding guarantees well-formedness")), u))
      )
    case TClass(Ref(cls)) if !generatingClasses.contains(cls) =>
      generatingClasses.having(cls) {
        foldStar(cls.collect { case f: InstanceField[Pre] => fieldTransitivePerm(e, f)(f.o) })
      }
    case TClass(Ref(cls)) =>
      // The class we are generating permission for has already been encountered when going through the chain
      // of fields. So we cut off the computation
      logger.warn(s"Not generating permissions for recursive occurrence of ${cls.o.debugName()}. Circular datastructures are not supported by permission generation")
      tt
    case _ => tt
  }

  def fieldTransitivePerm(`this`: Expr[Post], f: InstanceField[Pre])(implicit o: Origin): Expr[Post] = {
    fieldPerm[Post](`this`, succ(f), WritePerm()) &*
      transitivePerm(Deref[Post](`this`, succ(f))(PanicBlame("Permission for this field is already established")), f.t)
  }
}
