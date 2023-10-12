package vct.col.rewrite.lang

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast._
import vct.col.origin.{BlameCollector, Origin, InvocationFailure}
import vct.col.rewrite.{Generation, Rewritten}
import vct.col.origin.RedirectOrigin.StringReadable
import vct.col.ref.{LazyRef, Ref}
import vct.col.resolve.ctx.RefLlvmFunctionDefinition
import vct.col.util.SuccessionMap

case class LangLLVMToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) extends LazyLogging {
  type Post = Rewritten[Pre]
  implicit val implicitRewriter: AbstractRewriter[Pre, Post] = rw

  private val llvmFunctionMap: SuccessionMap[LlvmFunctionDefinition[Pre], Procedure[Post]] = SuccessionMap()
  private val specFunctionMap: SuccessionMap[LlvmSpecFunction[Pre], Function[Post]] = SuccessionMap()


  def rewriteLocal(local: LlvmLocal[Pre]): Expr[Post] = {
    implicit val o: Origin = local.o
    Local(rw.succ(local.ref.get.decl))
  }

  def rewriteFunctionDef(func: LlvmFunctionDefinition[Pre]): Unit = {
    implicit val o: Origin = func.o
    val procedure = rw.labelDecls.scope {
      rw.globalDeclarations.declare(
        new Procedure[Post](
          returnType = rw.dispatch(func.returnType),
          args = rw.variables.collect {
            func.args.foreach(rw.dispatch)
          }._1,
          outArgs = Nil,
          typeArgs = Nil,
          body = if (func.pure) Some(GotoEliminator(func.functionBody match { case scope: Scope[Pre] => scope }).eliminate()) else Some(rw.dispatch(func.functionBody)),
          contract = rw.dispatch(func.contract.data.get),
          pure = func.pure
        )(func.blame)
      )
    }
    llvmFunctionMap.update(func, procedure)
  }

  def rewriteAmbiguousFunctionInvocation(inv: LlvmAmbiguousFunctionInvocation[Pre]): Invocation[Post] = {
    implicit val o: Origin = inv.o
    inv.ref.get.decl match {
      case func: LlvmFunctionDefinition[Pre] => new ProcedureInvocation[Post](
        ref = new LazyRef[Post, Procedure[Post]](llvmFunctionMap(func)),
        args = inv.args.map(rw.dispatch),
        givenMap = inv.givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
        yields = inv.yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
        outArgs = Seq.empty,
        typeArgs = Seq.empty
      )(inv.blame)
      case func: LlvmSpecFunction[Pre] => new FunctionInvocation[Post](
        ref = new LazyRef[Post, Function[Post]](specFunctionMap(func)),
        args = inv.args.map(rw.dispatch),
        givenMap = inv.givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
        yields = inv.yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
        typeArgs = Seq.empty
      )(inv.blame)
    }

  }

  def rewriteFunctionInvocation(inv: LlvmFunctionInvocation[Pre]): ProcedureInvocation[Post] = {
    implicit val o: Origin = inv.o
    new ProcedureInvocation[Post](
      ref = new LazyRef[Post, Procedure[Post]](llvmFunctionMap(inv.ref.decl)),
      args = inv.args.map(rw.dispatch),
      givenMap = inv.givenMap.map { case (Ref(v), e) => (rw.succ(v), rw.dispatch(e)) },
      yields = inv.yields.map { case (e, Ref(v)) => (rw.dispatch(e), rw.succ(v)) },
      outArgs = Seq.empty,
      typeArgs = Seq.empty
    )(inv.blame)
  }

  def rewriteGlobal(decl: LlvmGlobal[Pre]): Unit = {
    implicit val o: Origin = decl.o
    rw.globalDeclarations.declare(
      decl.data match {
        case Some(data) => data match {
          case function: LlvmSpecFunction[Pre] =>
            val rwFunction = new Function[Post](
              rw.dispatch(function.returnType),
              rw.variables.collect {
                function.args.foreach(rw.dispatch)
              }._1,
              rw.variables.collect {
                function.typeArgs.foreach(rw.dispatch)
              }._1,
              function.body match {
                case Some(body) => Some(rw.dispatch(body))
                case None => None
              },
              rw.dispatch(function.contract),
              function.inline,
              function.threadLocal
            )(function.blame)
            specFunctionMap.update(function, rwFunction)
            rwFunction
        }
      })
  }

  def result(ref: RefLlvmFunctionDefinition[Pre])(implicit o: Origin): Expr[Post] =
    Result[Post](llvmFunctionMap.ref(ref.decl))

  /*
  Elimination works by replacing every goto with the block its referring too
  effectively transforming the CFG into a tree. More efficient restructuring algorithms but this works for now.

  This of course only works for acyclic CFGs as otherwise replacement would be infinitely recursive.
  Loop restructuring should be handled by VCLLVM as it has much more analytical and contextual information about
  the program.
  */
  case class GotoEliminator(bodyScope: Scope[Pre]) extends LazyLogging {
    val labelDeclMap: Map[LabelDecl[Pre], Label[Pre]] = bodyScope.body match {
      case block: Block[Pre] =>
        block.statements.map {
          case label: Label[Pre] => (label.decl, label)
        }.toMap
    }

    def eliminate(): Scope[Post] = {
      bodyScope match {
        case scope: Scope[Pre] => Scope[Post](
          rw.variables.collect {
            scope.locals.foreach(rw.dispatch)
          }._1,
          scope.body match {
            case bodyBlock: Block[Pre] => Block[Post](
              bodyBlock.statements.head match {
                case label: Label[Pre] => Seq(eliminate(label))
              }
            )(scope.body.o)
          }
        )(scope.o)
      }
    }

    def eliminate(label: Label[Pre]): Block[Post] = {
      implicit val o: Origin = label.o
      label.stat match {
        case block: Block[Pre] =>
          block.statements.last match {
            case goto: Goto[Pre] => Block[Post](block.statements.dropRight(1).map(rw.dispatch) ++ eliminate(labelDeclMap(goto.lbl.decl)).statements)
            case _: Return[Pre] => rw.dispatch(block) match {
              case block: Block[Post] => block
            }
            case branch: Branch[Pre] => Block[Post](block.statements.dropRight(1).map(rw.dispatch) :+ eliminate(branch))
          }
      }
    }

    def eliminate(branch: Branch[Pre]): Branch[Post] = {
      implicit val o: Origin = branch.o
      Branch[Post](
        branch.branches.map(
          bs => (rw.dispatch(bs._1), bs._2 match {
            case goto: Goto[Pre] => eliminate(labelDeclMap(goto.lbl.decl))
          })
        ))
    }
  }
}