package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.{Class, Declaration, InstanceField, Program}
import vct.col.rewrite.Generation
import vct.result.VerificationError.Unreachable


object FieldNumber {

  val instanceField: ScopedStack[InstanceField[_ <: Generation]] = ScopedStack()

  def apply[G <: Generation](declaration: InstanceField[G])(implicit program: Program[G]): Int = {
    instanceField.having(declaration) {
      val res = program.declarations.map(dispatchDeclaration).filter(a => a >= 0)
      if (res.size > 1) {
        throw Unreachable("Instance field cannot occur more than once")
      }
      res.lastOption.getOrElse(-1)
    }
  }

  private def dispatchDeclaration[G <: Generation](decl: Declaration[G]): Int = {
    decl match {
      case cls: Class[G] => dispatchClass(cls)
      case _ => -1
    }
  }

  private def dispatchClass[G <: Generation](cls: Class[G]): Int = {
    val allFields = cls.declarations.collect { case instancefieldDecl: InstanceField[G] => instancefieldDecl }
    val fieldsWithoutRuntime = allFields.filter(f => !f.o.getPreferredNameOrElse().contains("__runtime__"))
    fieldsWithoutRuntime.indexOf(instanceField.top)
  }

}
