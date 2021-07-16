package hre.util;

import static hre.lang.System.Verdict;

/**
 * This class contains a test report for a verification run.
 *
 * @author Stefan Blom
 */
public class TestReport {

    private Verdict verdict = Verdict.Pass;

    ;
    private Exception e;

    public Verdict getVerdict() {
        return verdict;
    }

    public void setVerdict(Verdict verdict) {
        this.verdict = verdict;
    }

    public Exception getException() {
        return e;
    }

    public void setException(Exception e) {
        this.verdict = Verdict.Error;
        this.e = e;
    }

    public void fail(String format, Object... args) {
        if (verdict != Verdict.Error) {
            verdict = Verdict.Fail;
        }
        Verdict("FAIL: " + format, args);
    }

    public static enum Verdict {Pass, Fail, Inconclusive, Error}

}
