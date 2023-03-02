package vct.logging;

public interface Message {

  Thread getThread();
  
  long nanoTime();

  void accept(MessageVisitor visitor);

  boolean isFatal();
 
}
