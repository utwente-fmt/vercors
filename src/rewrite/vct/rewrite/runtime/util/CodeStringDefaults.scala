package vct.rewrite.runtime.util

case object CodeStringDefaults {

  val newFieldPermissions: String = "public List<ConcurrentHashMap<Long, Fraction>> __runtime__ = new ArrayList<>(Arrays.asList(%s));"
  val newFieldConcurrentArray: String = "new ConcurrentHashMap<Long, Fraction>()"
}
