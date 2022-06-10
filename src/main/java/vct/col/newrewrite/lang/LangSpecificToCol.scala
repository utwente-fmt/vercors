package vct.col.newrewrite.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin._
import vct.col.resolve._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg}
import vct.result.VerificationError.UserError
import scala.collection.mutable

case object LangSpecificToCol extends RewriterBuilder {
  override def key: String = "langSpecific"
  override def desc: String = "Translate language-specific constructs to a common subset of nodes."

  case object ThisVar extends Origin {
    override def preferredName: String = "this"
    override def shortPosition: String = "generated"
    override def context: String = "[At node generated to store the this value for constructors]"
    override def inlineContext: String = "this"
  }

  case class NotAValue(value: Expr[_]) extends UserError {
    override def code: String = "notAValue"
    override def text: String = value.o.messageInContext("Could not resolve this expression to a value.")
  }

  case class BothFocusIgnore(d: Declaration[_]) extends UserError {
    override def code: String = "bothFocusIgnore"
    override def text: String = d.o.messageInContext("This declaration uses both focus and ignore")
  }
}

case class LangSpecificToCol[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging {
  val java: LangJavaToCol[Pre] = LangJavaToCol(this)
  val c: LangCToCol[Pre] = LangCToCol(this)
  val pvl: LangPVLToCol[Pre] = LangPVLToCol(this)
  val silver: LangSilverToCol[Pre] = LangSilverToCol(this)

  val currentThis: ScopedStack[Expr[Post]] = ScopedStack()
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()

  // TODO (RR): How to refer to constructors? What about overridden functions? Source-level annotation seems better...?
  val claimedMinimizeNames: mutable.Set[Seq[String]] = mutable.Set()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case model: Model[Pre] =>
      implicit val o: Origin = model.o
      currentThis.having(ThisModel[Post](succ(model))) {
        model.rewrite().succeedDefault(model)
      }

    case ns: JavaNamespace[Pre] => java.rewriteNamespace(ns)
    case cls: JavaClassOrInterface[Pre] => java.rewriteClass(cls)

    case cons: PVLConstructor[Pre] => pvl.rewriteConstructor(cons)

    case cParam: CParam[Pre] => c.rewriteParam(cParam)
    case func: CFunctionDefinition[Pre] => c.rewriteFunctionDef(func)
    case decl: CGlobalDeclaration[Pre] => c.rewriteGlobalDecl(decl)
    case decl: CDeclaration[Pre] => ???

    case cls: Class[Pre] =>
      currentClass.having(cls) {
        currentThis.having(ThisObject[Post](succ(cls))(cls.o)) {
          val decls = collectInScope(classScopes) {
            cls.declarations.foreach(dispatch)
            pvl.maybeDeclareDefaultConstructor(cls)
          }

          cls.rewrite(decls).succeedDefault(cls)
        }
      }

//    case m: InstanceMethod[Pre] =>
//      minimizePrefixNameLookup(Seq(currentClass.top.o.preferredName, m.o.preferredName)) match {
//        case Some(minimizeName) =>
//          claimedMinimizeNames.add(minimizeName)
//          m.rewrite(o = MinimizeOrigin(m.o, minimizeNames(minimizeName)))
//        case _ => m.rewrite()
//      }

    case other => rewriteDefault(other)
  }


  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case scope @ Scope(locals, body) =>
      def scanScope(node: Node[Pre]): Unit = node match {
        case Scope(_, _) =>
        case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration[Pre]) => java.declareLocal(locals)
        case other => other.subnodes.foreach(scanScope)
      }

      scope.rewrite(locals = collectInScope(variableScopes) {
        locals.foreach(dispatch)
        scanScope(body)
      })

    case JavaLocalDeclarationStatement(locals: JavaLocalDeclaration[Pre]) => java.initLocal(locals)

    case CDeclarationStatement(decl) => c.rewriteLocal(decl)
    case goto: CGoto[Pre] => c.rewriteGoto(goto)

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case result @ AmbiguousResult() =>
      implicit val o: Origin = result.o
      result.ref.get match {
        case ref: RefCFunctionDefinition[Pre] => c.result(ref)
        case ref: RefCGlobalDeclaration[Pre] => c.result(ref)
        case RefFunction(decl) => Result(succ(decl))
        case RefProcedure(decl) => Result(succ(decl))
        case RefJavaMethod(decl) => Result(succ(decl))
        case RefInstanceFunction(decl) => Result(succ(decl))
        case RefInstanceMethod(decl) => Result(succ(decl))
      }

    case diz @ AmbiguousThis() =>
      currentThis.top

    case local: JavaLocal[Pre] => java.local(local)
    case deref: JavaDeref[Pre] => java.deref(deref)
    case inv: JavaInvocation[Pre] => java.invocation(inv)
    case inv: JavaNewClass[Pre] => java.newClass(inv)
    case arr: JavaNewLiteralArray[Pre] => java.newLiteralArray(arr)
    case arr: JavaNewDefaultArray[Pre] => java.newDefaultArray(arr)
    case arr: JavaLiteralArray[Pre] => java.literalArray(arr)

    case local: PVLLocal[Pre] => pvl.local(local)
    case deref: PVLDeref[Pre] => pvl.deref(deref)
    case inv: PVLNew[Pre] => pvl.newClass(inv)
    case inv: PVLInvocation[Pre] => pvl.invocation(inv)

    case local: CLocal[Pre] => c.local(local)
    case inv: CInvocation[Pre] => c.invocation(inv)

    case inv: SilverPartialADTFunctionInvocation[Pre] => silver.adtInvocation(inv)
    case map: SilverUntypedNonemptyLiteralMap[Pre] => silver.nonemptyMap(map)

    case other => rewriteDefault(other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case t: JavaTClass[Pre] => java.classType(t)
    case other => rewriteDefault(other)
  }
}
