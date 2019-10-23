package hre.lang;

public class HREException extends Exception {

  /**
   * 
   */
  private static final long serialVersionUID = -1460088473914049731L;

  public HREException(String format,Object...args){
    super(String.format(format,args));
  }

}
