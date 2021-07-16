package hre.util;

import java.util.ArrayList;

/**
 * Test report that aggregates the results of several other tests.
 *
 * @author sccblom
 */
public class CompositeReport extends TestReport {

    private ArrayList<TestReport> reports = new ArrayList<TestReport>();

    public CompositeReport() {
        setVerdict(Verdict.Pass);
    }

    public void addReport(TestReport report) {
        reports.add(report);
        switch (report.getVerdict()) {
            case Error:
                setVerdict(Verdict.Error);
                break;
            case Fail:
                switch (getVerdict()) {
                    case Error:
                        break;
                    default:
                        setVerdict(report.getVerdict());
                        break;
                }
                break;
            case Inconclusive:
                switch (getVerdict()) {
                    case Error:
                    case Fail:
                        break;
                    default:
                        setVerdict(report.getVerdict());
                        break;
                }
                break;
            default:
                break;
        }
    }
}
