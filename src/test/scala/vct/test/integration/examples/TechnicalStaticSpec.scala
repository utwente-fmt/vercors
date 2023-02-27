package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalStaticSpec extends VercorsSpec{

  // displays that using String is possible and creates static field/method used in other tests
  vercors should verify using anyBackend example "technical/staticResolve/Test.java"

  // displays that using static fields from the same package is possible w/out explicit import
  vercors should verify using anyBackend example "technical/staticResolve/TestImport.java"

  // displays that without an explicit import, we cannot use static fields from classes outside our package
  vercors should error withCode "notAValue" example "technical/staticResolve/subPackage/ForgetImport.java"

  // displays that without an explicit import, we cannot use static methods from classes outside our package
  vercors should error withCode "noSuchName" example "technical/staticResolve/subPackage/ForgetMethodImport.java"

  // displays that with an explicit import, we can use both static fields and methods from other classes
  vercors should verify using anyBackend example "technical/staticResolve/subPackage/DidNotForgetImport.java"

  // displays that with a static import, we can use static fields and methods from other classes
  //  without needing to specify the fully qualified name of the method/field.
  vercors should verify using anyBackend example "technical/staticResolve/subPackage/StaticImport.java"
}
