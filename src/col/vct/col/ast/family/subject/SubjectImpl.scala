package vct.col.ast.family.subject

import vct.col.ast.{Subject, Class}
import vct.col.ast.ops.SubjectFamilyOps

trait SubjectImpl[G] extends SubjectFamilyOps[G] { this: Subject[G] =>
  def cls: Class[G]
}
