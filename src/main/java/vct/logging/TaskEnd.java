package vct.logging;

public class TaskEnd extends AbstractMessage {

  public final TaskBegin begin;
  
  public TaskEnd(TaskBegin begin) {
    this.begin=begin;
  }
  
  @Override
  public void accept(MessageVisitor visitor){
    visitor.visit(this);
  }


}
