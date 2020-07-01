package vct.col.util;

import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
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

  public void visit(Method m) {
    super.visit(m);

    ClassType throwableType = new ClassType(ClassType.javaLangThrowableName());

    for (Type t : m.throwy) {
      if (!(t instanceof ClassType)) {
        Fail("Throws type can only be class");
      }

      ClassType ct = (ClassType) t;
      if (!throwableType.supertypeof(source(), ct)) {
        Fail("Throws type must extend throwable");
      }
    }
  }

}
