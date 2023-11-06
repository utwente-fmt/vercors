package vct.rewrite.runtime.util

import vct.col.ast.{Class, InstanceField}





case class  FieldNumber[G](cls: Class[G]) {



  def findNumber(decl: InstanceField[G]): Int = {
    val allFields = cls.declarations.collect{case instancefield: InstanceField[G] => instancefield}
    val fieldsWithoutRuntime = allFields.filter(f => !f.o.getPreferredNameOrElse().contains("__runtime__"))
    fieldsWithoutRuntime.indexOf(decl)
  }

}
