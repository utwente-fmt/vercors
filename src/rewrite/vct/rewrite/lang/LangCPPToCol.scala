package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.ctx._
import vct.col.resolve.lang.CPP
import vct.col.rewrite.lang.LangSpecificToCol.NotAValue
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.UserError

case object LangCPPToCol {

  case class WrongCPPType(decl: CPPLocalDeclaration[_]) extends UserError {
    override def code: String = "wrongCPPType"

    override def text: String =
      decl.o.messageInContext(s"This declaration has a type that is not supported.")
  }

  case class CPPDoubleContracted(decl: CPPGlobalDeclaration[_], defn: CPPFunctionDefinition[_]) extends UserError {
    override def code: String = "multipleContracts"

    override def text: String =
      Origin.messagesInContext(Seq(
        defn.o -> "This method has a non-empty contract at its definition, ...",
        decl.o -> "... but its forward declaration also has a contract.",
      ))
  }

  case class KernelParFailure(kernel: CPPLambdaDefinition[_]) extends Blame[ParBlockFailure] {
    override def blame(error: ParBlockFailure): Unit = error match {
//      case ParPredicateNotInjective(_, predicate) =>
//        kernel.blame.blame(KernelPredicateNotInjective(kernel, predicate))
//      case ParPreconditionFailed(_, _) =>
//        PanicBlame("Kernel parallel block precondition cannot fail, since an identical predicate is required before.").blame(error)
//      case ParBlockPostconditionFailed(failure, _) =>
//        kernel.blame.blame(KernelPostconditionFailed(failure, kernel))
//      case ParBlockMayNotThrow(_) =>
//        PanicBlame("Please don't throw exceptions from a gpgpu kernel, it's not polite.").blame(error)
      case _ => PanicBlame("ELLEN: Implement blame!").blame(error)
    }
  }
}

case class LangCPPToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  import LangCPPToCol._
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val namespace: ScopedStack[CPPNamespaceDefinition[Pre]] = ScopedStack()
  val cppFunctionSuccessor: SuccessionMap[CPPFunctionDefinition[Pre], Procedure[Post]] = SuccessionMap()
  val cppFunctionDeclSuccessor: SuccessionMap[(CPPGlobalDeclaration[Pre], Int), Procedure[Post]] = SuccessionMap()
  val cppNameSuccessor: SuccessionMap[CPPNameTarget[Pre], Variable[Post]] = SuccessionMap()
  val cppGlobalNameSuccessor: SuccessionMap[CPPNameTarget[Pre], HeapVariable[Post]] = SuccessionMap()
  val cppCurrentDefinitionParamSubstitutions: ScopedStack[Map[CPPParam[Pre], CPPParam[Pre]]] = ScopedStack()
  val cppLambdaSuccessor: SuccessionMap[CPPLambdaDefinition[Pre], Procedure[Post]] = SuccessionMap()

  def rewriteUnit(cppUnit: CPPTranslationUnit[Pre]): Unit = {
    cppUnit.declarations.foreach(rw.dispatch)
  }

  def rewriteParam(cppParam: CPPParam[Pre]): Unit = {
    cppParam.drop()
    val varO = InterpretedOriginVariable(CPP.getDeclaratorInfo(cppParam.declarator).name, cppParam.o)

    val v = new Variable[Post](cppParam.specifiers.collectFirst
      { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get)(varO)
    cppNameSuccessor(RefCPPParam(cppParam)) = v
    rw.variables.declare(v)
  }

  def rewriteFunctionDef(func: CPPFunctionDefinition[Pre]): Unit = {
    func.drop()
    val info = CPP.getDeclaratorInfo(func.declarator)
    val infoParams = CPP.filterOutLambdaParams(info.params.get)
    val returnType = func.specs.collectFirst { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get

    val (contract, subs: Map[CPPParam[Pre], CPPParam[Pre]]) = func.ref match {
      case Some(RefCPPGlobalDeclaration(decl, idx)) if decl.decl.contract.nonEmpty =>
        if (func.contract.nonEmpty) throw CPPDoubleContracted(decl, func)
        // EW TODO: params are also filtered here, is ok?
        val declParams = CPP.getDeclaratorInfo(decl.decl.inits(idx).decl).params.get
        val defnParams = infoParams

        (decl.decl.contract, declParams.zip(defnParams).toMap)
      case _ =>
        (func.contract, Map.empty)
    }

    val namedO = InterpretedOriginVariable(CPP.getDeclaratorInfo(func.declarator).name, func.o)
    val proc =
      cppCurrentDefinitionParamSubstitutions.having(subs) {
        rw.globalDeclarations.declare(
          {
            val params = rw.variables.collect {
              infoParams.foreach(rw.dispatch)
            }._1
            rw.labelDecls.scope {
              new Procedure[Post](
                returnType = returnType,
                args = params,
                outArgs = Nil,
                typeArgs = Nil,
                body = Some(rw.dispatch(func.body)),
                contract = rw.dispatch(contract),
              )(func.blame)(namedO)
            }
          }
        )
      }

    cppFunctionSuccessor(func) = proc

    func.ref match {
      case Some(RefCPPGlobalDeclaration(decl, idx)) =>
        cppFunctionDeclSuccessor((decl, idx)) = proc
      case None => // ok
    }
  }

  def rewriteLambdaDef(lambda: CPPLambdaDefinition[Pre], body: Statement[Pre]): Unit = {
    val info = CPP.getDeclaratorInfo(lambda.declarator)
    val returnType = TVoid[Post]()

    val namedO = InterpretedOriginVariable("VERCORS_LAMBDA_METHOD", lambda.o)
    val proc =
      cppCurrentDefinitionParamSubstitutions.having(Map.empty) {
        rw.globalDeclarations.declare(
          {
            val params = rw.variables.collect {
              CPP.filterOutLambdaParams(info.params.get).foreach(rw.dispatch)
            }._1
            rw.labelDecls.scope {
              new Procedure[Post](
                returnType = returnType,
                args = params,
                outArgs = Nil,
                typeArgs = Nil,
                body = Some(rw.dispatch(body)),
                contract = rw.dispatch(lambda.contract),
              )(lambda.blame)(namedO)
            }
          }
        )
      }

    cppLambdaSuccessor(lambda) = proc

//    val returnExpr = new CPPLambdaRef[Post]()(namedO)
//    returnExpr.ref = Some(new RefProcedure[Post](proc))
//    returnExpr
  }

  def rewriteGlobalDecl(decl: CPPGlobalDeclaration[Pre]): Unit = {
    val t = decl.decl.specs.collectFirst { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get
    for ((init, idx) <- decl.decl.inits.zipWithIndex if init.ref.isEmpty) {
      // If the reference is empty, skip the declaration: the definition is used instead.
      val info = CPP.getDeclaratorInfo(init.decl)
      val namedO = InterpretedOriginVariable(info.name, init.o)
      info.params match {
        case Some(params) =>
          cppFunctionDeclSuccessor((decl, idx)) = rw.globalDeclarations.declare(
            new Procedure[Post](
              returnType = t,
              args = rw.variables.collect {
                CPP.filterOutLambdaParams(params).foreach(rw.dispatch)
              }._1,
              outArgs = Nil,
              typeArgs = Nil,
              body = None,
              contract = rw.dispatch(decl.decl.contract),
            )(AbstractApplicable)(namedO)
          )
        case None =>
          cppGlobalNameSuccessor(RefCPPGlobalDeclaration(decl, idx)) =
            rw.globalDeclarations.declare(new HeapVariable(t)(namedO))
      }
    }
  }

  def rewriteLocal(decl: CPPLocalDeclaration[Pre]): Statement[Post] = {
    decl.drop()
    val t = decl.decl.specs.collectFirst { case t: CPPSpecificationType[Pre] => rw.dispatch(t.t) }.get
    decl.decl.specs.foreach {
      case _: CPPSpecificationType[Pre] =>
      case _ => throw WrongCPPType(decl)
    }

    // LangTypesToCol makes it so that each declaration only has one init
    val init = decl.decl.inits.head

    val info = CPP.getDeclaratorInfo(init.decl)
    val varO: Origin = InterpretedOriginVariable(info.name, init.o)
    t match {
      case cta @ CPPTArray(Some(size), t) =>
        if (init.init.isDefined) throw WrongCPPType(decl)
        implicit val o: Origin = init.o
        val v = new Variable[Post](TArray(t))(varO)
        cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
        val newArr = NewArray[Post](t, Seq(size), 0)(cta.blame)
        Block(Seq(LocalDecl(v), assignLocal(v.get, newArr)))
      case _ =>
        val v = new Variable[Post](t)(varO)
        cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
        implicit val o: Origin = init.o
        init.init
          .map(value => Block(Seq(LocalDecl(v), assignLocal(v.get, rw.dispatch(value)))))
          .getOrElse(LocalDecl(v))
    }
  }

  def rewriteNamespaceDef(ns: CPPNamespaceDefinition[Pre]): Unit = {
    ns.drop()
    namespace.having(ns) {
      // Do not enter a scope, so methods of the namespace are declared globally to the program.
      ns.declarations.foreach(rw.dispatch)
    }
  }

  def result(ref: RefCPPFunctionDefinition[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](cppFunctionSuccessor.ref(ref.decl))

  def result(ref: RefCPPLambdaDefinition[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](cppLambdaSuccessor.ref(ref.decl))

  def result(ref: RefCPPGlobalDeclaration[Pre])(implicit o: Origin): Expr[Post] = {
    val maybeDefinition = ref.decls.decl.inits(ref.initIdx).ref
    maybeDefinition match {
      case Some(defn) => Result[Post](cppFunctionSuccessor.ref(defn.decl))
      case None => Result[Post](cppFunctionDeclSuccessor.ref((ref.decls, ref.initIdx)))
    }
  }

  def local(local: CPPLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    local.ref.get match {
      case RefAxiomaticDataType(_) => throw NotAValue(local)
      case RefVariable(decl) => Local(rw.succ(decl))
      case RefModelField(decl) => ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(local.blame)
      case ref: RefCPPParam[Pre] =>
        if (cppCurrentDefinitionParamSubstitutions.nonEmpty)
          Local(cppNameSuccessor.ref(RefCPPParam(cppCurrentDefinitionParamSubstitutions.top.getOrElse(ref.decl, ref.decl))))
        else
          Local(cppNameSuccessor.ref(ref))
      case RefCPPFunctionDefinition(_) => throw NotAValue(local)
      case ref @ RefCPPGlobalDeclaration(decl, initIdx) =>
        CPP.getDeclaratorInfo(decl.decl.inits(initIdx).decl).params match {
          case None => DerefHeapVariable[Post](cppGlobalNameSuccessor.ref(ref))(local.blame)
          case Some(_) => throw NotAValue(local)
        }
      case ref: RefCPPLocalDeclaration[Pre] => Local(cppNameSuccessor.ref(ref))
    }
  }

  def invocation(inv: CPPInvocation[Pre]): Expr[Post] = {
    val CPPInvocation(applicable, args, givenMap, yields) = inv
    implicit val o: Origin = inv.o
    inv.ref.get match {
      case RefFunction(decl) =>
        FunctionInvocation[Post](rw.succ(decl), args.map(rw.dispatch), Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case RefProcedure(decl) =>
        ProcedureInvocation[Post](rw.succ(decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case RefPredicate(decl) =>
        PredicateApply[Post](rw.succ(decl), args.map(rw.dispatch), WritePerm())
      case RefInstanceFunction(decl) => ???
      case RefInstanceMethod(decl) => ???
      case RefInstancePredicate(decl) => ???
      case RefADTFunction(decl) =>
        ADTFunctionInvocation[Post](None, rw.succ(decl), args.map(rw.dispatch))
      case RefModelProcess(decl) =>
        ProcessApply[Post](rw.succ(decl), args.map(rw.dispatch))
      case RefModelAction(decl) =>
        ActionApply[Post](rw.succ(decl), args.map(rw.dispatch))
      case BuiltinInstanceMethod(f) => ???
      case ref: RefCPPFunctionDefinition[Pre] =>
        var filteredArgs = Seq[Expr[Pre]]()
        args.foreach {
          case argument: CPPLambdaDefinition[Pre] =>
          case argument => filteredArgs = filteredArgs :+ argument
        }
        ProcedureInvocation[Post](cppFunctionSuccessor.ref(ref.decl), filteredArgs.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case ref: RefCPPLambdaDefinition[Pre] =>
        ProcedureInvocation[Post](cppLambdaSuccessor.ref(ref.decl), args.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) })(inv.blame)
      case e: RefCPPGlobalDeclaration[Pre] => globalInvocation(e, inv)
      case RefProverFunction(decl) => ProverFunctionInvocation(rw.succ(decl), args.map(rw.dispatch))
    }
  }

  def globalInvocation(e: RefCPPGlobalDeclaration[Pre], inv: CPPInvocation[Pre]): Expr[Post] = {
    val CPPInvocation(_, args, givenMap, yields) = inv
    val RefCPPGlobalDeclaration(decls, initIdx) = e
    implicit val o: Origin = inv.o

    val arg = if (args.size == 1) {
      args.head match {
        case IntegerValue(i) if i >= 0 && i < 3 => Some(i.toInt)
        case _ => None
      }
    } else None
    // Process lambda parameters and remove them from the parameter list
    var filteredArgs = Seq[Expr[Pre]]()
    args.foreach {
      case argument: CPPLambdaDefinition[Pre] =>
      case argument => filteredArgs = filteredArgs :+ argument
    }
    (e.name, arg) match {
      case _ => {
        var procedureRef: Ref[Post, Procedure[Post]] = cppFunctionDeclSuccessor.ref((decls, initIdx))
        procedureRef = rewritePotentialSYCLProcedure(e, procedureRef, inv)
        ProcedureInvocation[Post](
          procedureRef, filteredArgs.map(rw.dispatch), Nil, Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) }
        )(inv.blame)
      }
    }
  }

  def rewritePotentialSYCLProcedure(declaration: RefCPPGlobalDeclaration[Pre], declProcedureRef: Ref[Post, Procedure[Post]], invocation: CPPInvocation[Pre]): Ref[Post, Procedure[Post]] = {
    // cppLambdaSuccessor(invocation.args(0))
    // EW TODO: better method matching
    if (declaration.name == "submit" && CPP.getPrimitiveType(declaration.decls.decl.specs).isInstanceOf[SYCLTEvent[Pre]]) {
      // Command group submit method
//      val commandGroupProcedure = cppLambdaSuccessor(invocation.args.head.asInstanceOf[CPPLambdaDefinition[Pre]])
      val lambdaDef: CPPLambdaDefinition[Pre] = invocation.args.head.asInstanceOf[CPPLambdaDefinition[Pre]]
      rewriteLambdaDef(lambdaDef, lambdaDef.body)
      val mew = 5;
    } else if (declaration.name == "parallel_for" && CPP.getPrimitiveType(declaration.decls.decl.specs).isInstanceOf[TVoid[Pre]]) {
      // Kernel parallel_for
      val lambdaDef: CPPLambdaDefinition[Pre] = invocation.args.toSeq(1).asInstanceOf[CPPLambdaDefinition[Pre]]
      implicit val o: Origin = lambdaDef.body.o
      val innerBody = lambdaDef.body.asInstanceOf[Scope[Pre]].body.asInstanceOf[Block[Pre]]
      val blockDecl = new ParBlockDecl[Pre]()
      val UnitAccountedPredicate(contractRequires: Expr[Pre]) = lambdaDef.contract.requires
      val UnitAccountedPredicate(contractEnsures: Expr[Pre]) = lambdaDef.contract.ensures
      val newOuterBody = ParStatement[Pre](ParBlock[Pre](
        decl = blockDecl,
        iters = Seq(),
        // Context is already inherited
        context_everywhere = lambdaDef.contract.contextEverywhere,
        requires = contractRequires,
        ensures = contractEnsures,
        content = Scope[Pre](Seq(), Block[Pre](Seq(innerBody))),
      )(KernelParFailure(lambdaDef)))

      rewriteLambdaDef(lambdaDef, newOuterBody)
//      val kernelProcedure = cppLambdaSuccessor(invocation.args.toSeq(1).asInstanceOf[CPPLambdaDefinition[Pre]])
//      kernelProcedure.body.get.body
      val mew = 5;
    }
    declProcedureRef
  }
//
//  def pointerType(t: CPPTPointer[Pre]): Type[Post] = {
//    TPointer(rw.dispatch(t.innerType))
//  }

  def arrayType(t: CPPTArray[Pre]): Type[Post] = {
    // TODO: we should not use pointer here
    TPointer(rw.dispatch(t.innerType))
  }

  // EW TODO: remove?
  def lambdaType(t: CPPTLambda[Pre]): Type[Post] = TRef()

}