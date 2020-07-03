package vct.col.util;

import vct.col.ast.stmt.composite.CatchClause;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.logging.PassReport;

import java.util.ArrayList;

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

    // Pure methods cannot throw exceptions
    if (m.getKind() == Method.Kind.Pure) {
      if (m.getContract().signals.length > 0 || m.throwy.length > 0) {
        Fail("Pure methods cannot throw exceptions");
      }
    }

    // Throws types must inherit from Throwable
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

  public void visit(TryCatchBlock tcb) {
    super.visit(tcb);

    ArrayList<ClassType> encounteredCatchTypes = new ArrayList<>();
    ClassType throwableType = new ClassType(ClassType.javaLangThrowableName());

    for (CatchClause cc : tcb.catches()) {
      enter(cc);
      ArrayList<ClassType> encounteredMultiCatchTypes = new ArrayList<>();

      for (Type catchType : cc.javaCatchTypes()) {
        if (!(catchType instanceof ClassType)) {
          Fail("Catch clause types must be class types");
        }

        ClassType ct = (ClassType) catchType;

        if (!throwableType.supertypeof(source(), ct)) {
          Fail("Catch clause types must inherit from Throwable");
        }

        for (ClassType t : encounteredMultiCatchTypes) {
          if (t.supertypeof(source(), ct)) {
            Fail("Types within a multi-catch cannot be subtypes");
          }
        }
        encounteredMultiCatchTypes.add(ct);

        for (Type t : encounteredCatchTypes) {
          if (t.supertypeof(source(), ct)) {
            Fail("Catch type %s is already caught by earlier catch clause", ct);
          }
        }
        encounteredCatchTypes.add(ct);
      }

      leave(cc);
    }
  }

}
