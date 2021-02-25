package hre.io;

/**
 * A message has a message type and arguments.
 * The message type is given in the form of a format
 * to allow pretty printing. Note that several standards for format strings
 * exists. E.g. the Java standard, the C standard.
 * More may be defined if a portable encoding is needed.
 * However, that is future work.
 * 
 * @author Stefan Blom.
 */
public class Message {

  private String format;
  private Object args[];
  public Message(String format, Object ... args){
    this.format=format;
    this.args=args;
  }
  
  public String getFormat(){
    return format;
  }
  
  public Object[] getArgs(){
    return args;
  }
  
  public Object getArg(int i){
    return args[i];
  }

  public String getFormattedMessage() {
    return String.format(format, args);
  }
}

