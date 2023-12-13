package vct.col.ast.family.subject

import vct.col.ast.{Subject, Class}

trait SubjectImpl[G] { this: Subject[G] =>
  def cls: Class[G]
}
