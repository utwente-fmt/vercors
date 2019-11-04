package hre.lang;

public class HREError extends Error {

  /**
   * 
   */
  private static final long serialVersionUID = -1460088473914049732L;

  public HREError(String format,Object...args){
    super(String.format(format,args));
  }

}
