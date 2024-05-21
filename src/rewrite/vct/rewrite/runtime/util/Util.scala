package vct.rewrite.runtime.util


import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.FrozenScopes
import vct.result.VerificationError.Unreachable

object Util {

  /**
   * Collects all the classes that are extending a Thread
   * @param declarations
   * @tparam G
   * @return
   */
  def collectThreadClasses[G](declarations: Seq[Declaration[G]]): Seq[Class[G]] = {
    declarations.collect { case c: Class[G] => c }.filter(isExtendingThread)
  }

  /**
   * get the run method of a start/join MethodInvocation
   * @param mi
   * @tparam G
   * @return
   */
  def getRunMethod[G](mi: MethodInvocation[G]): InstanceMethod[G] = {
    if (!isThreadMethod(mi, "start") && !isThreadMethod(mi, "join")) {
      throw Unreachable("Looking up the run method requires a start/join method as input")
    }
    getRunMethod(getClassOfMethodInvocation(mi))
  }

  /**
   * Get the run method of the class
   * @param cls
   * @tparam G
   * @return
   */
  def getRunMethod[G](cls: Class[G]): InstanceMethod[G] = {
    if (!isExtendingThread(cls)) {
      throw Unreachable("Can only get a run method if the class is extending a Thread")
    }
    cls.declarations.collect { case i: InstanceMethod[G] => i }.find(i => isMethod(i, "run")).getOrElse({
      throw Unreachable("Extending Thread class should have a run method")
    })
  }

  /**
   * Does the method has the method name
   * @param i
   * @param methodName
   * @tparam G
   * @return
   */
  def isMethod[G](i: InstanceMethod[G], methodName: String): Boolean = {
    i.o.getPreferredNameOrElse() == methodName
  }

  /**
   * Is the class extending a Thread
   * @param cls
   * @tparam G
   * @return
   */
  def isExtendingThread[G](cls: Class[G]): Boolean = {
    cls.supports.collectFirst { case TClass(Ref(cls), _) if cls.o.getPreferredNameOrElse().ucamel.contains("Thread") => () }.nonEmpty
  }

  /**
   * is the method a method with a certain name and is the class of the method extending a thread
   * @param mi
   * @param methodName
   * @tparam G
   * @return
   */
  def isThreadMethod[G](mi: MethodInvocation[G], methodName: String): Boolean = {
    isMethod(mi.ref.decl, methodName) && isExtendingThread(getClassOfMethodInvocation(mi))
  }

  /**
   *
   * @param mi
   * @tparam G
   * @return
   */
  private def getClassOfMethodInvocation[G](mi: MethodInvocation[G]): Class[G] = {
    mi.obj match {
      case l: Local[G] => l.ref.decl.t.asInstanceOf[TClass[G]].cls.decl
      case d: Deref[G] => d.ref.decl.t.asInstanceOf[TClass[G]].cls.decl
      case _ => ???
    }
  }

  /**
   * Collects the method body
   * @param i
   * @tparam G
   * @return
   */
  def collectMethodBody[G](i: InstanceMethod[G]): Scope[G] = {
    i.body match {
      case Some(sc: Scope[G]) => sc
      case _ => ???
    }
  }

  /**
   * Collects the block of the scope
   * @param s
   * @tparam G
   * @return
   */
  def collectBlockScope[G](s: Scope[G]): Block[G] = {
    s.body match {
      case b: Block[G] => b
      case _ => ???
    }
  }

  /**
   * Converts a permission to a runtime value
   * @param perm
   * @param origin
   * @tparam G
   * @return
   */
  def permissionToRuntimeValue[G](perm: Perm[G])(implicit origin: Origin): Expr[G] = {
    permissionToRuntimeValue(perm.perm)
  }

  /**
   * Converts a permission expression to a runtime value
   * @param expr
   * @param origin
   * @tparam G
   * @return
   */
  def permissionToRuntimeValue[G](expr: Expr[G])(implicit origin: Origin): Expr[G] = {
    expr match {
      case _: WritePerm[G] => RuntimeFractionOne()
      case _: ReadPerm[G] => RuntimeFractionZero()
      case d: RatDiv[G] => RuntimeFractionDiff[G](d.left, d.right)
      case d: FloorDiv[G] => RuntimeFractionDiff[G](d.left, d.right)
      case IntegerValue(n: BigInt) if n == 1 => RuntimeFractionOne()
      case _ => expr
    }
  }

  /**
   * Converts a permission to a runtime value and dispatches it to the next generation
   * @param permission
   * @param origin
   * @tparam Pre
   * @return
   */
  def permissionToRuntimeValueRewrite[Pre <: Generation](permission: Perm[Pre])(implicit origin: Origin): Expr[Rewritten[Pre]] = {
    type Post = Rewritten[Pre]
    val rw = new Rewriter[Pre]()
    val oldExpr: Expr[Pre] = permissionToRuntimeValue(permission.perm)
    rw.dispatch(oldExpr)
  }

  /**
   * Returns the closest injectivity map, these are always present in the methods
   * @param sc
   * @tparam Pre
   * @return
   */
  def findClosestInjectivityMap[Pre <: Generation](sc: FrozenScopes[Pre, Rewritten[Pre], Variable[Pre], Variable[Rewritten[Pre]]]): Variable[Rewritten[Pre]] = {
    sc.scopes.collectFirst{
      case m if m.values.exists(v => v.o.getPreferredNameOrElse() == "injectivityMap") => m.values.find(v => v.o.getPreferredNameOrElse() == "injectivityMap").get
    }.get
  }

}
