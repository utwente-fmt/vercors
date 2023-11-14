package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.{Class, Declaration, InstanceField, Program, TClass}
import vct.result.VerificationError.Unreachable





case class  FieldNumber[G](prog: Program[G]) {

  val instanceField: ScopedStack[InstanceField[G]] = ScopedStack()

  def findNumber(decl: InstanceField[G]): Int = {
    instanceField.having(decl){
      val res = prog.declarations.map(dispatch).filter(a => a >= 0)
      if(res.size > 1) {
        throw Unreachable("Instance field cannot occur more than once")
      }
      res.lastOption.getOrElse(-1)
    }
  }

  private def dispatch(decl: Declaration[G]): Int = {
    decl match {
      case cls: Class[G] => dispatch(cls)
      case _ => -1
    }
  }

  private def dispatch(cls: Class[G]): Int = {
    val allFields = cls.declarations.collect { case instancefieldDecl: InstanceField[G] => instancefieldDecl }
    val fieldsWithoutRuntime = allFields.filter(f => !f.o.getPreferredNameOrElse().contains("__runtime__"))
    fieldsWithoutRuntime.indexOf(instanceField.top)
  }

}
