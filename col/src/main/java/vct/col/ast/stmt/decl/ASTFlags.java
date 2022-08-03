package vct.col.ast.stmt.decl;

public interface ASTFlags {

  int STATIC=0x0001;
  
  int GHOST=0x0002;
  
  int IN_ARG = 0x0004;
  
  int OUT_ARG = 0x0008;
  
  int FINAL = 0x0010;
  
  int INLINE = 0x0020;

  int PUBLIC = 0x0040;

  int THREAD_LOCAL = 0x0080;

  int EXTERN = 0x0100;

  int UNIQUE = 0x0200;

}
