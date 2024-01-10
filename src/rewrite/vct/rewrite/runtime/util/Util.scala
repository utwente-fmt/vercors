package vct.rewrite.runtime.util


import vct.col.ast._
import vct.col.util.AstBuildHelpers.findAllDerefs
import vct.result.VerificationError.Unreachable


object Util {



  def isExtendingThread[G](cls: Class[G]) : Boolean = {
    cls.supports.map(s => s.decl.o.getPreferredNameOrElse()).contains("Thread")
  }

  def collectThreadClasses[G](declarations: Seq[Declaration[G]]) : Seq[Class[G]] ={
    declarations.collect{case c: Class[G] => c}.filter(isExtendingThread)
  }

  def isRunMethod[G](i: InstanceMethod[G]) : Boolean = {
    i.o.getPreferredNameOrElse() == "run"
  }

  def isStartMethod[G](im: InstanceMethod[G]): Boolean = {
    im.o.getPreferredNameOrElse() == "start"
  }

  def getRunMethod[G](cls: Class[G]) : InstanceMethod[G] = {
    if(!isExtendingThread(cls)) {
      throw Unreachable("Can only get a run method if the class is extending a Thread")
    }
    cls.declarations.collect { case i: InstanceMethod[G] => i }.find(isRunMethod).getOrElse({
      throw Unreachable("Extending Thread class should have a run method")
    })
  }

  def isStartThreadMethod[G](mi: MethodInvocation[G]): Boolean = {
    isExtendingThread(getClassOfMethodInvocation(mi)) && isStartMethod(mi.ref.decl)
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


  def getRunMethod[G](mi: MethodInvocation[G]): InstanceMethod[G] = {
    if(!isStartThreadMethod(mi)) {
      throw Unreachable("Looking up the run method requires a start method as input")
    }
    getRunMethod(getClassOfMethodInvocation(mi))
  }



}
