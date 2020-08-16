package vct.main;

import vct.col.ast.stmt.decl.ProgramUnit;
import vct.logging.ExceptionMessage;
import vct.logging.PassAddVisitor;
import vct.logging.PassReport;
import hre.lang.HREError;
import hre.lang.HREException;
import hre.util.TestReport;
import hre.util.TestReport.Verdict;

public abstract class ValidationPass extends Pass {
    public ValidationPass(String description) {
        super(description);
    }

    public PassReport apply_pass(PassReport report, String... args) {
        ProgramUnit arg = report.getOutput();
        PassReport result = new PassReport(arg);
        result.setOutput(arg);
        result.add(new PassAddVisitor(report));

        TestReport tr = apply(arg, args);
        if (tr.getVerdict() != Verdict.Pass) {
            result.add(new ExceptionMessage(new HREException("pass %s yielded %s", getDescripion(), tr.getVerdict())));
        }
        return result;
    }

    protected TestReport apply(ProgramUnit arg, String... args) {
        throw new HREError("Incorrectly implemented validation pass %s", this.getClass());
    }
}
