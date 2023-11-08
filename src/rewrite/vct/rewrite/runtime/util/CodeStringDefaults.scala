package vct.rewrite.runtime.util

import vct.col.ast.Expr

import scala.reflect.internal.NoPhase.id

case object CodeStringDefaults {

  val newFieldPermissions: String => String = (hashMaps: String) => s"public List<ConcurrentHashMap<Long, Fraction>> __runtime__ = new ArrayList<>(Arrays.asList($hashMaps));"
  val newFieldConcurrentArray: String = "new ConcurrentHashMap<Long, Fraction>()"
  val assertCheckRead: (Int, String) => String = (id: Int, field: String) => s"assert(__runtime__.get($id).get(Thread.currentThread().getId() > 0) : ${'"'}Permission for field: $field is not enough${'"'};"
  val assertCheckWrite: (Int, String) => String = (id: Int, field: String) => s"assert(__runtime__.get($id).get(Thread.currentThread().getId() == 1) : ${'"'}Permission for field: $field should be write not is not write${'"'};"
  val assertPermissionCondition: (Int, String) => String = (id: Int, perm: String) => s"assert(__runtime__.get($id).get(Thread.currentThread().getId() == $perm)"
  val fractionTemplate : (String, String) => String = (numerator: String, denominator: String) => s"Fraction.getFraction($numerator,$denominator)"
}
