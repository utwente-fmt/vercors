package vct.logging;

public abstract class AbstractMessage implements Message {
  
  private long time=System.nanoTime();
  private Thread thread=Thread.currentThread();
 
  protected boolean fatal;

  @Override
  public Thread getThread() {
    return thread;
  }

  @Override
  public long nanoTime() {
    return time;
  }

  @Override
  public boolean isFatal(){
    return fatal;
  }
}
