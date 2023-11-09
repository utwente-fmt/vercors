package vct.rewrite.runtime

import vct.col.ast.{Block, Class, CodeStringStatement, ContractApplicable, Declaration, Deref, Expr, InstanceField, InstanceMethod, MethodInvocation, PostAssignExpression, PreAssignExpression, Program, Result, Scope, Statement, Type, Variable}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.rewrite.runtime.util.CodeStringDefaults.{assertCheckRead, assertCheckWrite, lookUpThread}
import vct.rewrite.runtime.util.FieldNumber

import scala.collection.mutable.HashMap

object CheckPermissionsBlocksMethod extends RewriterBuilder {
  override def key: String = "CheckPermissionsBlocksMethod"

  override def desc: String = "Creates internal method blocks. In these blocks permissions will be checked"
}


case class CheckPermissionsBlocksMethod[Pre <: Generation]() extends Rewriter[Pre] {

  private var seenMethodInvocation: Boolean = false
  private val givenMethodSucc: SuccessionMap[InstanceMethod[Pre], InstanceMethod[Post]] = SuccessionMap()

  var first = false
  var target = false
  val occurences = new HashMap[LazyRef[Pre, InstanceField[Pre]], Boolean]()

  var fieldFinder: FieldNumber[Pre] = null

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val test = super.dispatch(program)
    test
  }

  private def createNewInstanceMethod(im: InstanceMethod[Pre], newBlock: Block[Post], newLocals: Seq[Variable[Post]], sc: Scope[Pre]): InstanceMethod[Post] = {
    val newScope = Some(Scope(newLocals, newBlock)(sc.o))
    new InstanceMethod[Post](dispatch(im.returnType),
      variables.collect(im.args.foreach(a => dispatch(a)))._1,
      variables.collect(im.outArgs.foreach(a => dispatch(a)))._1,
      variables.collect(im.typeArgs.foreach(a => dispatch(a)))._1,
      newScope,
      dispatch(im.contract),
      im.inline,
      im.pure
    )(im.blame)(im.o)

  }


  private def determineBlocks(b: Block[Pre]): Seq[Block[Pre]] = {
    val newBlocks = b.statements.foldLeft((Seq.empty[Block[Pre]], Block[Pre](Seq())(b.o))) {
      case ((blocks, tmpBlock), s) =>
        rewriteDefault(s)
        if (seenMethodInvocation) {
          seenMethodInvocation = false
          (blocks :+ tmpBlock :+ Block[Pre](Seq(s))(b.o), Block[Pre](Seq())(b.o))
        } else {
          (blocks, Block[Pre](tmpBlock.statements :+ s)(b.o))
        }
    }
    newBlocks._1 :+ newBlocks._2
  }


  private def dispatchTarget(t: Expr[Pre]): Expr[Post] = {
    target = true
    dispatch(t)
  }

  private def dispatchValue(v: Expr[Pre]): Expr[Post] = {
    target = false
    dispatch(v)
  }

  private def generatePermissionChecksStatements(ref: LazyRef[Pre, InstanceField[Pre]], bool: Boolean, b: Block[Pre]): CodeStringStatement[Post] = {
    val id: Int = fieldFinder.findNumber(ref.decl)
    val name: String = ref.decl.o.getPreferredNameOrElse()
    if (bool) {
      CodeStringStatement[Post](assertCheckWrite(id, name))(b.o)
    } else {
      CodeStringStatement[Post](assertCheckRead(id, name))(b.o)
    }
  }


  private def dispatchGivenMethodBlock(b: Block[Pre]): Block[Post] = {
    occurences.clear()
    val newStatements: Seq[Statement[Post]] = b.statements.map(dispatch)
    val assertionStatements: Seq[Statement[Post]] = occurences.map(pair => generatePermissionChecksStatements(pair._1, pair._2, b)).toSeq
    val allNewStatements: Seq[Statement[Post]] = assertionStatements ++ newStatements
    Block[Post](allNewStatements)(b.o)
  }


  private def determineNewMethodStructure(b: Block[Pre]): Block[Post] = {
    val methodBlocks: Seq[Block[Pre]] = determineBlocks(b)
    val newMethodBlocks: Seq[Statement[Post]] = methodBlocks.map(dispatchGivenMethodBlock)
    Block[Post](newMethodBlocks)(b.o)
  }


  private def dispatchGivenMethod(im: InstanceMethod[Pre]): Unit = {
    im.body match {
      case Some(sc: Scope[Pre]) => {
        sc.body match {
          case b: Block[Pre] => {
            val newLocals = variables.collect(sc.locals.foreach(l => dispatch(l)))._1
            val newIm = createNewInstanceMethod(im, determineNewMethodStructure(b), newLocals, sc)
            classDeclarations.declare(newIm)
            givenMethodSucc.update(im, newIm)
          }
          case e => rewriteDefault(e)
        }
      }
      case _ => rewriteDefault(im)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case mi: MethodInvocation[Pre] => {
        seenMethodInvocation = true
        new MethodInvocation[Post](
          dispatch(mi.obj),
          givenMethodSucc.ref[Post, InstanceMethod[Post]](mi.ref.decl),
          mi.args.map(a => dispatch(a)),
          mi.outArgs.map(a => dispatch(a)),
          mi.typeArgs.map(a => dispatch(a)),
          Seq(),
          Seq()
        )(mi.blame)(mi.o)
      }
      case preAssign: PreAssignExpression[Pre] => {
        dispatchTarget(preAssign.target)
        dispatchValue(preAssign.value)
        super.rewriteDefault(e)
      }
      case postAssign: PostAssignExpression[Pre] => {
        dispatchTarget(postAssign.target)
        dispatchValue(postAssign.value)
        super.rewriteDefault(e)
      }
      case res: Result[Pre] => {
        res.applicable.decl match {
          case im: InstanceMethod[Pre] => {
            Result[Post](givenMethodSucc.ref(im))(res.o)
          }
          case _ => rewriteDefault(res)
        }
      }
      case d: Deref[Pre] => {
        d.ref match {
          case lr: LazyRef[Pre, InstanceField[Pre]] => {
            if (target) {
              occurences += (lr -> true)
            } else if (!occurences.contains(lr)) {
              occurences += (lr -> false)
            }
          }
          case _ =>
        }
        super.rewriteDefault(d)

      }
      case _ => rewriteDefault(e)
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case im: InstanceMethod[Pre] => dispatchGivenMethod(im)
      case cls: Class[Pre] => {
        fieldFinder = FieldNumber[Pre](cls)
        rewriteDefault(cls)
      }
      case _ => rewriteDefault(decl)
    }
  }
}