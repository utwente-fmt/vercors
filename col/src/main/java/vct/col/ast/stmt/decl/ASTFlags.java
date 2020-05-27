package vct.col.ast.stmt.decl;

public interface ASTFlags {

  public final int STATIC=0x0001;
  
  public final int GHOST=0x0002;
  
  public final int  IN_ARG = 0x0004;
  
  public final int OUT_ARG = 0x0008;
  
  public final int   FINAL = 0x0010;
  
  public final int  INLINE = 0x0020;

  public final int  PUBLIC = 0x0040;

  public final int THREAD_LOCAL = 0x0080;

  public final int EXTERN = 0x0100;

  public final int UNIQUE = 0x0200;

}
