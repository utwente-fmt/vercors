package vct.rewrite.runtime.util

import vct.col.ast.Expr

import scala.reflect.internal.NoPhase.id

case object CodeStringDefaults {

  // Creating new fields
  val newFieldPermissions: String => String = (hashMaps: String) => s"public List<ConcurrentHashMap<Long, Fraction>> __runtime__ = new ArrayList<>(Arrays.asList($hashMaps));"
  val newFieldConcurrentArray: String = "new ConcurrentHashMap<Long, Fraction>()"
  val newArrayPermission: (Int) => String = (id: Int) => s"public List<ConcurrentHashMap<Long, Fraction>> __runtime__$id = new ArrayList<>();"
  val initNewArrayPermissions: (Int, String) => String = (id: Int, hashMaps: String) => s"public List<ConcurrentHashMap<Long, Fraction>> __runtime__$id = new ArrayList<>(Arrays.asList($hashMaps));"
  val generatedHashMapsFunction : String = "private void __generateHashMap__(int size, List<ConcurrentHashMap<Long, Fraction>> arrayMap ) {\n\t\tarrayMap.clear();\n\t\tfor(int i = 0; i < size; i ++) {\n\t\t\tarrayMap.add(new ConcurrentHashMap<Long, Fraction>());\n\t\t}\n\t}"
  val callGenerateHashMaps : (String, Int) => String = (size: String, id: Int) => s"__generateHashMap__($size, __runtime__$id)"
  val generateHashMapCreation : (Int) => String = (size: Int) => (1 to size).map(_ => CodeStringDefaults.newFieldConcurrentArray).mkString(", ")

  // Assertion checking
  val assertCheck: (String, Int, String, Boolean) => String = (objectLocation: String, id: Int, field: String, write: Boolean) => {
    if (write) {
      assertCheckWrite(objectLocation, id, field)
    } else {
      assertCheckRead(objectLocation, id, field)
    }
  }
  val assertCheckRead: (String, Int, String) => String = (objectLocation: String, id: Int, field: String) => s"assert(${objectLocation}.__runtime__.get($id).get(Thread.currentThread().getId() > 0) : ${'"'}Permission for field: $field is not enough${'"'};"
  val assertCheckWrite: (String, Int, String) => String = (objectLocation: String, id: Int, field: String) => s"assert(${objectLocation}.__runtime__.get($id).get(Thread.currentThread().getId() == 1) : ${'"'}Permission for field: $field should be write not is not write${'"'};"
  val assertPermissionCondition: (String, Int, String) => String = (objectLocation: String, id: Int, perm: String) => s"assert(${objectLocation}.__runtime__.get($id).get(Thread.currentThread().getId() == $perm)"
  val fractionTemplate: (String, String) => String = (numerator: String, denominator: String) => s"Fraction.getFraction($numerator,$denominator)"
  val lookUpThread: String = "Long __runtime_thread_id__ = Thread.currentThread().getId();"
  val assertCondition : String => String = (expr: String) => s"assert($expr);"

  // Forking
  val takePermissionInteger : (String, Int, String) => String = (objectLocation: String, id: Int, value: String) => s"${objectLocation}.__runtime__.get(${id}).put(Thread.currentThread().getId(), ${value})"
  val takePermissionWrite : (String, Int) => String = (objectLocation: String, id: Int) => s"${objectLocation}.__runtime__.get(${id}).put(Thread.currentThread().getId(), 1)"
  val takePermissionRead : (String, Int) => String = (objectLocation: String, id: Int) => s"${objectLocation}.__runtime__.get(${id}).put(Thread.currentThread().getId(), ${fractionTemplate("1","1000000")})"
  val takePermissionDiv : (String, Int, String) => String = (objectLocation: String, id: Int, value: String) => s"${objectLocation}.__runtime__.get(${id}).put(Thread.currentThread().getId(), ${value})"


  //Quantifiers
  val quantifierTemplate : (String, Int, Int,String) => String = (variable: String, lowerBound: Int, upperBound: Int, body) => s"for(int $variable = $lowerBound; $variable <= $upperBound; $variable++){\n\t$body\n}"
  val methodTemplate : (String, String, String) => String = (quantifierId: String, body: String, paramaters: String) => s"public void __runtime__quantifier__$quantifierId($paramaters) {" +
    s"$body" +
    s"}"
  val loopConditionTemplate: (String, String) => String = (loopCondition: String, body: String) => s"if($loopCondition){\n$body\n}"
  val callMethodTemplate: (String, String) => String = (quantifierId: String, paramaters: String) => s"__runtime__quantifier__$quantifierId($paramaters)"
}
