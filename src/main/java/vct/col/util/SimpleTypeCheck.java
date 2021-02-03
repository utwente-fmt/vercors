package vct.col.util;

import vct.col.ast.stmt.decl.ProgramUnit;
import vct.logging.PassReport;

/**
 * This class implements type checking of simple object oriented programs.
 * 
 * An object oriented programs is simple if it does not use overloading.
 * 
 * @author Stefan Blom
 *
 */
public class SimpleTypeCheck extends AbstractTypeCheck {

  public SimpleTypeCheck(PassReport report, ProgramUnit arg){
    super(report, arg);
  }

}
