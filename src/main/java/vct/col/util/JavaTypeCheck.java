package vct.col.util;

import vct.col.ast.stmt.decl.ProgramUnit;
import vct.logging.PassReport;

/**
 * This class implements type checking of object oriented programs
 * that may use inheritance and/or overloading.
 * 
 * @author Stefan Blom
 *
 */
public class JavaTypeCheck extends AbstractTypeCheck {

  public JavaTypeCheck(PassReport report, ProgramUnit arg) {
    super(report, arg);
  }

}
