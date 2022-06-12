package vct.test.integration.examples

import vct.col.ast.{Eq, Function, Program, Result, SplitAccountedPredicate, TInt, UnitAccountedPredicate}
import vct.col.origin.{DiagnosticOrigin, PanicBlame}
import vct.col.print.Printer
import vct.col.rewrite.InitialGeneration
import vct.test.integration.helper.VercorsSpec
import vct.col.util.AstBuildHelpers._

class XX extends VercorsSpec {
  type G = InitialGeneration
  implicit val o = DiagnosticOrigin

  def mkFn(failing: Boolean, focus: Boolean, ignore: Boolean): Function[G] =
      new Function(
        TInt(), Seq(), Seq(), Some(const[G](0)), contract(PanicBlame(""), ensures=UnitAccountedPredicate(const(!failing))),
        focus = focus, ignore = ignore)(null)

  val p = new Program[G](
    Seq(
      mkFn(true, false, false),
      mkFn(false, true, false)
    ), null)(null)

  val sb = new java.lang.StringBuilder
  val printer = Printer(sb, syntax = vct.col.print.PVL)
  printer.print(p)
  println(sb.toString)
}

class TechnicalMinimizeSpec extends VercorsSpec {
  vercors should verify using anyBackend example "technical/minimize/FocusMethod.java"
  vercors should verify using anyBackend example "technical/minimize/FocusMethodTransitive.java"
  vercors should verify using anyBackend example "technical/minimize/FocusFunction.java"
  vercors should verify using anyBackend example "technical/minimize/FocusFunctionTransitive.java"
  vercors should verify using anyBackend example "technical/minimize/IgnoreFunction.java"
  vercors should verify using anyBackend example "technical/minimize/IgnoreMethod.java"

  vercors should verify using anyBackend example "technical/minimize/IgnoreConstructor.java"
  vercors should verify using anyBackend example "technical/minimize/IgnoreConstructor.pvl"
  vercors should verify using anyBackend example "technical/minimize/FocusConstructor.java"
  vercors should verify using anyBackend example "technical/minimize/FocusConstructor.pvl"

  vercors should verify using anyBackend example "technical/minimize/FocusTopLevelFunction.java"
  vercors should verify using anyBackend example "technical/minimize/FocusTopLevelFunction.pvl"
  vercors should verify using anyBackend example "technical/minimize/FocusTopLevelProcedure.pvl"


//  vercors should verify using anyBackend in "example asserting this instanceof the defining class" java """
//    class MyClass {
//      void foo() {
//        MyClass myClass = new MyClass();
//        assert myClass instanceof MyClass;
//      }
//    }
//  """
}
