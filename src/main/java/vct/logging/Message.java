package vct.logging;

public interface Message {

  public Thread getThread();
  
  public long nanoTime();

  public void accept(MessageVisitor visitor);

  public boolean isFatal();
 
}
