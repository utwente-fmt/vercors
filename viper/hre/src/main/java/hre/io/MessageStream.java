package hre.io;

import java.io.PrintStream;

/**
 * This class represents a way of sending text messages.
 * 
 * The current implementation assume that all messages are sent to
 * a PrintStream. Future implementations will be able to use
 * logging frameworks as well.
 * 
 */
public class MessageStream {
  private PrintStream out;
  private String tag;
  public MessageStream(PrintStream out,String tag){
    this.out=out;
    this.tag=tag;
  }
  public void say(String format,Object...args){
    String message=String.format(format,args);
    out.printf("%s: %s%n",tag,message);
  }
}