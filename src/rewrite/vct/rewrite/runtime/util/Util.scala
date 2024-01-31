package vct.rewrite.runtime.util


import vct.col.ast._
import vct.col.origin.{InstancePredicateClassRuntime, Origin}
import vct.col.rewrite.{Rewritten, Generation}
import vct.result.VerificationError.Unreachable

object Util {


  def collectThreadClasses[G](declarations: Seq[Declaration[G]]): Seq[Class[G]] = {
    declarations.collect { case c: Class[G] => c }.filter(isExtendingThread)
  }

  def getRunMethod[G](mi: MethodInvocation[G]): InstanceMethod[G] = {
    if (!isThreadMethod(mi, "start") && !isThreadMethod(mi, "join")) {
      throw Unreachable("Looking up the run method requires a start/join method as input")
    }
    getRunMethod(getClassOfMethodInvocation(mi))
  }

  def getRunMethod[G](cls: Class[G]): InstanceMethod[G] = {
    if (!isExtendingThread(cls)) {
      throw Unreachable("Can only get a run method if the class is extending a Thread")
    }
    cls.declarations.collect { case i: InstanceMethod[G] => i }.find(i => isMethod(i, "run")).getOrElse({
      throw Unreachable("Extending Thread class should have a run method")
    })
  }

  def isMethod[G](i: InstanceMethod[G], methodName: String): Boolean = {
    i.o.getPreferredNameOrElse() == methodName
  }

  def isExtendingThread[G](cls: Class[G]): Boolean = {
    cls.supports.map(s => s.decl.o.getPreferredNameOrElse()).contains("Thread")
  }

  def isThreadMethod[G](mi: MethodInvocation[G], methodName: String): Boolean = {
    isMethod(mi.ref.decl, methodName) && isExtendingThread(getClassOfMethodInvocation(mi))
  }

  private def getClassOfMethodInvocation[G](mi: MethodInvocation[G]): Class[G] = {
    mi.obj match {
      case l: Local[G] => l.ref.decl.t match {
        case ct: TClass[G] => ct.cls.decl
        case _ => {
          println(l.ref.decl.t)
          ???
        }
      }
      case d: Deref[G] => {
        d.ref.decl.t match {
          case ct: TClass[G] => ct.cls.decl
          case _ => ???
        }
      }
      case _ => ???
    }
  }


  def collectMethodBody[G](i: InstanceMethod[G]): Scope[G] = {
    i.body match {
      case Some(sc: Scope[G]) => sc
      case _ => ???
    }
  }

  def collectBlockScope[G](s: Scope[G]): Block[G] = {
    s.body match {
      case b: Block[G] => b
      case _ => ???
    }
  }


  case class InstancePredicateData[G](ipa: InstancePredicateApply[G], cls: Class[G], ip: InstancePredicate[G], predicateClass: Class[G], args: Seq[Expr[G]])(implicit program: Program[G]) {
    def createMethodInvocation(methodName: String)(implicit origin: Origin): MethodInvocation[G] = {
      val instanceMethod: InstanceMethod[G] = findInstancePredicateFunction[G](predicateClass, methodName)
      val staticRef = StaticClassRef[G](predicateClass.ref)(predicateClass.o)
      MethodInvocation[G](
        staticRef,
        instanceMethod.ref,
        args,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty
      )(null)
    }
  }

  def findInstancePredicateData[G](ipa: InstancePredicateApply[G])(implicit program: Program[G]) : InstancePredicateData[G] = {
    val cls: Class[G] = ipa.obj.t.asInstanceOf[TClass[G]].cls.decl
    val ip: InstancePredicate[G] = ipa.ref.decl
    val predicateClass: Class[G] = findInstancePredicateClass[G](cls, ip)
    val args: Seq[Expr[G]] = ipa.args :+ ipa.obj
    InstancePredicateData[G](ipa, cls, ip, predicateClass, args)
  }

  def findInstancePredicateClass[G](cls: Class[G], ip: InstancePredicate[G])(implicit program: Program[G]) : Class[G] = {
    val allClasses = program.declarations.collect{case c: Class[G] => c}
    val allOrigins = allClasses.map(c => c.o.getInstancePredicateClassRuntime)
    allClasses.zip(allOrigins).collectFirst{
      case (c: Class[G], Some(InstancePredicateClassRuntime(a, b))) if a == cls.o.getPreferredNameOrElse() && b == ip.o.getPreferredNameOrElse() => c
    }.get
  }

  def findInstancePredicateFunction[G](cls: Class[G], name: String)(implicit program: Program[G]) : InstanceMethod[G] = {
    cls.declarations.collectFirst{case im: InstanceMethod[G] if im.o.getPreferredNameOrElse() == name => im}.get
  }



}
