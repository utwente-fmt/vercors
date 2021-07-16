package vct.logging;

public class TaskPhase extends AbstractMessage {


    public final TaskBegin subTask;

    public final String description;

    public TaskPhase(TaskBegin subTask, String description) {
        this.subTask = subTask;
        this.description = description;
    }

    @Override
    public void accept(MessageVisitor visitor) {
        visitor.visit(this);
    }


}
