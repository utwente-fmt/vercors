package vct.logging;

public class PassAddVisitor implements MessageVisitor {

    private final PassReport report;

    public PassAddVisitor(PassReport report) {
        this.report = report;
    }

    @Override
    public void visit(ExceptionMessage e) {
        report.add(e);
    }

    @Override
    public void visit(TaskBegin begin) {
        report.add(begin);
    }

    @Override
    public void visit(TaskEnd end) {
        report.add(end);
    }

    @Override
    public void visit(TaskPhase phase) {
        report.add(phase);
    }

    @Override
    public void visit(VerificationResult result) {
        report.add(result);
    }

    @Override
    public void visit(VerCorsError error) {
        report.add(error);
    }

}
