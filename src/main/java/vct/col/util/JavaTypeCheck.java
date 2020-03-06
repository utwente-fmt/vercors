package vct.col.util;

import hre.ast.Origin;
import vct.col.ast.stmt.composite.CatchClause;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.Type;
import vct.logging.PassAddVisitor;
import vct.logging.PassReport;
import vct.logging.VerCorsError;
import vct.silver.ErrorDisplayVisitor;
import viper.api.ViperError;
import viper.api.ViperErrorImpl;

import java.util.ArrayList;

import static hre.lang.System.Output;

/**
 * This class implements type checking of Java
 *
 * @author Stefan Blom
 *
 */
public class JavaTypeCheck extends AbstractTypeCheck {

  public JavaTypeCheck(ProgramUnit arg) {
    super(arg);
  }

  @Override
  public void visit(TryCatchBlock tcb) {
    super.visit(tcb);

    ArrayList<Type> caughtTypes = new ArrayList<>();
    for (CatchClause catchClause : tcb.catches()) {
      Type caughtType = catchClause.decl().type();
      if (caughtTypes.contains(caughtType)) {
        ViperErrorImpl<Origin> error = new ViperErrorImpl<Origin>(tcb.getOrigin(), "method.precondition.unsound:method.precondition.false");

        PassReport passReport = new PassReport(source());
        passReport.setOutput(source());
        passReport.add(new ErrorDisplayVisitor());
        passReport.add(new VerCorsError(VerCorsError.ErrorCode.NotWellFormed, VerCorsError.SubCode.OverlappingCatchClauseTypes, tcb.getOrigin(), new ArrayList<>(0)));
        hre.lang.System.Fail("Cannot catch duplicate types");
      }
      caughtTypes.add(caughtType);
    }
  }
}
