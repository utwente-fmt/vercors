package vct.test.integration

import vct.test.integration.helper.VercorsSpec

class TechnicalAbruptSpec extends VercorsSpec {
  vercors should verify using silicon example "technical/exception-patterns/CatchBreak.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchContinue.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchEmpty.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchIf.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchLog.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchNestedTry.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchReturn.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchStackTrace.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchSwitch.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchThrowE.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchThrowNewE.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchThrowNewEWrap.java"
  vercors should verify using silicon example "technical/exception-patterns/CatchWhile.java"
  vercors should verify using silicon example "technical/exception-patterns/FinallyContinue.java"
  vercors should verify using silicon example "technical/exception-patterns/FinallyEmpty.java"
  vercors should verify using silicon example "technical/exception-patterns/FinallyLog.java"
  vercors should verify using silicon example "technical/exception-patterns/FinallyNestedTry.java"
  vercors should verify using silicon example "technical/exception-patterns/FinallyReturn.java"
  vercors should verify using silicon example "technical/exception-patterns/FinallyThrowNewE.java"
}
