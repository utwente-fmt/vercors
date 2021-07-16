package vct.logging;

import hre.util.TestReport;
import hre.util.TestReport.Verdict;
import vct.col.ast.stmt.decl.ProgramUnit;

import java.util.ArrayList;
import java.util.HashSet;

import static hre.lang.System.Output;

public class PassReport {

    private ArrayList<Message> entries = new ArrayList<Message>();
    private ProgramUnit input;
    private ProgramUnit output;
    private int fatal = 0;
    private HashSet<MessageVisitor> visitors = new HashSet<MessageVisitor>();

    public PassReport(ProgramUnit in) {
        input = in;
    }

    public PassReport(ProgramUnit in, TestReport report) {
        input = in;
        output = in;
        if (report.getVerdict() != Verdict.Pass) {
            fatal = 1;
        }
    }

    public void add(Message m) {
        entries.add(m);
        if (m.isFatal()) fatal++;
        for (MessageVisitor v : visitors) {
            m.accept(v);
        }
    }

    public ProgramUnit setOutput(ProgramUnit pu) {
        return output = pu;
    }

    public ProgramUnit getOutput() {
        return output;
    }

    public ProgramUnit getInput() {
        return input;
    }

    public int getFatal() {
        return fatal;
    }

    public void fatal(String format, Object... args) {
        Output(format, args);
        fatal++;
    }

    public void add(MessageVisitor visitor) {
        visitors.add(visitor);
    }

    public void remove(MessageVisitor visitor) {
        visitors.remove(visitor);
    }

    public void listFatals() {
        Output("fatal count is %d", fatal);
        for (Message m : entries) {
            if (m.isFatal()) {
                Output("fatal entry %s", m.getClass());
                Output("fatal entry %s", m);
            }
        }
    }
}
