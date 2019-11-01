package vct.logging;

public class TaskBegin extends AbstractMessage {


  public final TaskBegin subTask;
  
  public final String description;
  
  public TaskBegin(String description){
    this(null,description);
  }
  
  public TaskBegin(TaskBegin subTask, String description) {
    this.subTask=subTask;
    this.description=description;
  }

  @Override
  public void accept(MessageVisitor visitor){
    visitor.visit(this);
  }


}
