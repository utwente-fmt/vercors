package vct.col.util;

import vct.col.ast.stmt.composite.CatchClause;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.logging.PassReport;
import vct.logging.VerCorsError;
import vct.logging.VerCorsError.ErrorCode;
import vct.logging.VerCorsError.SubCode;

import java.util.ArrayList;

/**
 * This class implements type checking of Java
 *
 * @author Stefan Blom
 *
 */
public class JavaTypeCheck extends AbstractTypeCheck {
  PassReport passReport;

  public JavaTypeCheck(PassReport report, ProgramUnit arg) {
    super(arg);
    this.passReport = report;

    ASTClass throwableClass = source().find("java_DOT_lang_DOT_Throwable");
    if (throwableClass != null) {
      throwableType = throwableClass.toClassType();
    } else {
      throwableType = null;
    }
  }

  ClassType throwableType;

  @Override
  public void visit(TryCatchBlock tcb) {
    super.visit(tcb);

    ArrayList<Type> caughtTypes = new ArrayList<>();
    for (CatchClause catchClause : tcb.catches()) {
      Type caughtType = catchClause.decl().type();
      if (caughtTypes.contains(caughtType)) {
        report(
                new VerCorsError(ErrorCode.NotWellFormed, SubCode.OverlappingCatchClauseTypes, tcb.getOrigin(), new ArrayList<>(0)),
                "Cannot catch duplicate types"
        );
      }
      caughtTypes.add(caughtType);
    }
  }

  public void visit(Method method) {
    super.visit(method);

    for (Type type : method.getThrowsTypes()) {
      if (throwableType == null || !throwableType.supertypeof(source(), type)) {
        report(
                new VerCorsError(
                        ErrorCode.NotWellFormed,
                        SubCode.InvalidExceptionType,
                        type.getOrigin(),
                        new ArrayList<>(0)
                ),
                "Type in throws has to extend Throwable or be a subclass of Throwable"
        );
      }
    }
  }

  public void report(VerCorsError error, String msg) {
    passReport.add(error);
    hre.lang.System.Fail(msg);
  }
}
