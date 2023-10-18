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
      decl.o
        .messageInContext(s"This declaration has a type that is not supported.")
  }

  case class CPPDoubleContracted(
      decl: CPPGlobalDeclaration[_],
      defn: CPPFunctionDefinition[_],
  ) extends UserError {
    override def code: String = "multipleContracts"

    override def text: String =
      Origin.messagesInContext(Seq(
        defn.o -> "This method has a non-empty contract at its definition, ...",
        decl.o -> "... but its forward declaration also has a contract.",
      ))
  }
}

case class LangCPPToCol[Pre <: Generation](rw: LangSpecificToCol[Pre])
    extends LazyLogging {
  import LangCPPToCol._
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  val namespace: ScopedStack[CPPNamespaceDefinition[Pre]] = ScopedStack()
  val cppFunctionSuccessor
      : SuccessionMap[CPPFunctionDefinition[Pre], Procedure[Post]] =
    SuccessionMap()
  val cppFunctionDeclSuccessor
      : SuccessionMap[(CPPGlobalDeclaration[Pre], Int), Procedure[Post]] =
    SuccessionMap()
  val cppNameSuccessor: SuccessionMap[CPPNameTarget[Pre], Variable[Post]] =
    SuccessionMap()
  val cppGlobalNameSuccessor
      : SuccessionMap[CPPNameTarget[Pre], HeapVariable[Post]] = SuccessionMap()
  val cppCurrentDefinitionParamSubstitutions
      : ScopedStack[Map[CPPParam[Pre], CPPParam[Pre]]] = ScopedStack()

  def rewriteUnit(cppUnit: CPPTranslationUnit[Pre]): Unit = {
    cppUnit.declarations.foreach(rw.dispatch)
  }

  def rewriteParam(cppParam: CPPParam[Pre]): Unit = {
    cppParam.drop()
    val varO = InterpretedOriginVariable(
      CPP.getDeclaratorInfo(cppParam.declarator).name,
      cppParam.o,
    )

    val v =
      new Variable[Post](cppParam.specifiers.collectFirst {
        case t: CPPSpecificationType[Pre] => rw.dispatch(t.t)
      }.get)(varO)
    cppNameSuccessor(RefCPPParam(cppParam)) = v
    rw.variables.declare(v)
  }

  def rewriteFunctionDef(func: CPPFunctionDefinition[Pre]): Unit = {
    func.drop()
    val info = CPP.getDeclaratorInfo(func.declarator)
    val returnType =
      func.specs.collectFirst { case t: CPPSpecificationType[Pre] =>
        rw.dispatch(t.t)
      }.get

    val (contract, subs: Map[CPPParam[Pre], CPPParam[Pre]]) =
      func.ref match {
        case Some(RefCPPGlobalDeclaration(decl, idx))
            if decl.decl.contract.nonEmpty =>
          if (func.contract.nonEmpty)
            throw CPPDoubleContracted(decl, func)

          val declParams =
            CPP.getDeclaratorInfo(decl.decl.inits(idx).decl).params.get
          val defnParams = info.params.get

          (decl.decl.contract, declParams.zip(defnParams).toMap)
        case _ => (func.contract, Map.empty)
      }

    val namedO = InterpretedOriginVariable(
      CPP.getDeclaratorInfo(func.declarator).name,
      func.o,
    )
    val proc =
      cppCurrentDefinitionParamSubstitutions.having(subs) {
        rw.globalDeclarations.declare({
          val params =
            rw.variables.collect { info.params.get.foreach(rw.dispatch) }._1
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
        })
      }

    cppFunctionSuccessor(func) = proc

    func.ref match {
      case Some(RefCPPGlobalDeclaration(decl, idx)) =>
        cppFunctionDeclSuccessor((decl, idx)) = proc
      case None => // ok
    }
  }

  def rewriteGlobalDecl(decl: CPPGlobalDeclaration[Pre]): Unit = {
    val t =
      decl.decl.specs.collectFirst { case t: CPPSpecificationType[Pre] =>
        rw.dispatch(t.t)
      }.get
    for ((init, idx) <- decl.decl.inits.zipWithIndex if init.ref.isEmpty) {
      // If the reference is empty, skip the declaration: the definition is used instead.
      val info = CPP.getDeclaratorInfo(init.decl)
      info.params match {
        case Some(params) =>
          cppFunctionDeclSuccessor((decl, idx)) = rw.globalDeclarations.declare(
            new Procedure[Post](
              returnType = t,
              args = rw.variables.collect { params.foreach(rw.dispatch) }._1,
              outArgs = Nil,
              typeArgs = Nil,
              body = None,
              contract = rw.dispatch(decl.decl.contract),
            )(AbstractApplicable)(init.o)
          )
        case None =>
          cppGlobalNameSuccessor(RefCPPGlobalDeclaration(decl, idx)) = rw
            .globalDeclarations.declare(new HeapVariable(t)(init.o))
      }
    }
  }

  def rewriteLocal(decl: CPPLocalDeclaration[Pre]): Statement[Post] = {
    decl.drop()
    val t =
      decl.decl.specs.collectFirst { case t: CPPSpecificationType[Pre] =>
        rw.dispatch(t.t)
      }.get
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
        if (init.init.isDefined)
          throw WrongCPPType(decl)
        implicit val o: Origin = init.o
        val v = new Variable[Post](TArray(t))(varO)
        cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
        val newArr = NewArray[Post](t, Seq(size), 0)(cta.blame)
        Block(Seq(LocalDecl(v), assignLocal(v.get, newArr)))
      case _ =>
        val v = new Variable[Post](t)(varO)
        cppNameSuccessor(RefCPPLocalDeclaration(decl, 0)) = v
        implicit val o: Origin = init.o
        init.init.map(value =>
          Block(Seq(LocalDecl(v), assignLocal(v.get, rw.dispatch(value))))
        ).getOrElse(LocalDecl(v))
    }
  }

  def rewriteNamespaceDef(ns: CPPNamespaceDefinition[Pre]): Unit = {
    ns.drop()
    namespace.having(ns) {
      // Do not enter a scope, so methods of the namespace are declared globally to the program.
      ns.declarations.foreach(rw.dispatch)
    }
  }

  def result(ref: RefCPPFunctionDefinition[Pre])(implicit
      o: Origin
  ): Expr[Post] = Result[Post](cppFunctionSuccessor.ref(ref.decl))

  def result(
      ref: RefCPPGlobalDeclaration[Pre]
  )(implicit o: Origin): Expr[Post] = {
    val maybeDefinition = ref.decls.decl.inits(ref.initIdx).ref
    maybeDefinition match {
      case Some(defn) => Result[Post](cppFunctionSuccessor.ref(defn.decl))
      case None =>
        Result[Post](cppFunctionDeclSuccessor.ref((ref.decls, ref.initIdx)))
    }
  }

  def local(local: CPPLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    local.ref.get match {
      case RefAxiomaticDataType(_) => throw NotAValue(local)
      case RefVariable(decl) => Local(rw.succ(decl))
      case RefModelField(decl) =>
        ModelDeref[Post](rw.currentThis.top, rw.succ(decl))(local.blame)
      case ref: RefCPPParam[Pre] =>
        if (cppCurrentDefinitionParamSubstitutions.nonEmpty)
          Local(cppNameSuccessor.ref(RefCPPParam(
            cppCurrentDefinitionParamSubstitutions.top
              .getOrElse(ref.decl, ref.decl)
          )))
        else
          Local(cppNameSuccessor.ref(ref))
      case RefCPPFunctionDefinition(_) => throw NotAValue(local)
      case ref @ RefCPPGlobalDeclaration(decl, initIdx) =>
        CPP.getDeclaratorInfo(decl.decl.inits(initIdx).decl).params match {
          case None =>
            DerefHeapVariable[Post](cppGlobalNameSuccessor.ref(ref))(
              local.blame
            )
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
        FunctionInvocation[Post](
          rw.succ(decl),
          args.map(rw.dispatch),
          Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
        )(inv.blame)
      case RefProcedure(decl) =>
        ProcedureInvocation[Post](
          rw.succ(decl),
          args.map(rw.dispatch),
          Nil,
          Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
        )(inv.blame)
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
        ProcedureInvocation[Post](
          cppFunctionSuccessor.ref(ref.decl),
          args.map(rw.dispatch),
          Nil,
          Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
        )(inv.blame)
      case e: RefCPPGlobalDeclaration[Pre] => globalInvocation(e, inv)
      case RefProverFunction(decl) =>
        ProverFunctionInvocation(rw.succ(decl), args.map(rw.dispatch))
    }
  }

  def globalInvocation(
      e: RefCPPGlobalDeclaration[Pre],
      inv: CPPInvocation[Pre],
  ): Expr[Post] = {
    val CPPInvocation(_, args, givenMap, yields) = inv
    val RefCPPGlobalDeclaration(decls, initIdx) = e
    implicit val o: Origin = inv.o

    val arg =
      if (args.size == 1) {
        args.head match {
          case IntegerValue(i) if i >= 0 && i < 3 => Some(i.toInt)
          case _ => None
        }
      } else
        None
    (e.name, arg) match {
      case _ =>
        ProcedureInvocation[Post](
          cppFunctionDeclSuccessor.ref((decls, initIdx)),
          args.map(rw.dispatch),
          Nil,
          Nil,
          givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
          yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
        )(inv.blame)
    }
  }

  def arrayType(t: CPPTArray[Pre]): Type[Post] = {
    // TODO: we should not use pointer here
    TPointer(rw.dispatch(t.innerType))
  }

}
