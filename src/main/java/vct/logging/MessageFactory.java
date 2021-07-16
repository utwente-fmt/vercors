package vct.logging;

import hre.ast.Origin;
import viper.api.ViperError;


public class MessageFactory {

    public final MessageVisitor visitor;

    public MessageFactory(MessageVisitor visitor) {
        this.visitor = visitor;
    }

    public void exception(Exception exception) {
        visitor.visit(new ExceptionMessage(exception));
    }

    public TaskBegin begin(String description) {
        TaskBegin res = new TaskBegin(description);
        visitor.visit(res);
        return res;
    }

    public void end(TaskBegin task) {
        visitor.visit(new TaskEnd(task));
    }

    public void phase(TaskBegin task, String description) {
        visitor.visit(new TaskPhase(task, description));
    }

    public void error(ViperError<Origin> e) {
        visitor.visit(VerCorsError.viper_error(e));
    }

    public void error(VerCorsError e) {
        visitor.visit(e);
    }

    public void result(boolean pass, Origin origin) {
        visitor.visit(new VerificationResult(pass, origin));
    }

}
