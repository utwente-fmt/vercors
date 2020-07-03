package vct.col.util;

import hre.ast.Origin;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.CatchClause;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.decl.SignalsClause;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.logging.MessageFactory;
import vct.logging.PassAddVisitor;
import vct.logging.PassReport;
import vct.logging.VerCorsError;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

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

  public void visit(SignalsClause sc) {
    super.visit(sc);

    ClassType throwableType = new ClassType(ClassType.javaLangThrowableName());
    if (!throwableType.supertypeof(source(), sc.type())) {
      reportFail("Signals type must extend Throwable",
              VerCorsError.ErrorCode.TypeError, VerCorsError.SubCode.ExtendsThrowable,
              sc.getType(), sc);
    }
  }

  public void reportFail(VerCorsError e) {
    if (report != null) {
      MessageFactory log=new MessageFactory(new PassAddVisitor(report));
      log.error(e);
    } else {
      Abort("Report was null: %s", e);
    }
  }

  public void reportFail(String message, VerCorsError.ErrorCode ec, VerCorsError.SubCode sc, ASTNode primaryOrigin, ASTNode... secondaryOrigins) {
    List<Origin> auxOrigin = Arrays.stream(secondaryOrigins).map(n -> n.getOrigin()).collect(Collectors.toList());
    reportFail(new VerCorsError(
            VerCorsError.ErrorCode.TypeError,
            VerCorsError.SubCode.ExtendsThrowable,
            primaryOrigin.getOrigin(),
            auxOrigin
    ));
    Fail(message);
  }

  public void visit(TryCatchBlock tcb) {
    // TODO: And that, if checked exception, exception appears in try body! (this should probably go in the java type checker)
    // TODO: Multi-catch
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
