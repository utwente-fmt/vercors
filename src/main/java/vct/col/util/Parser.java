package vct.col.util;

import java.io.File;

import vct.col.ast.stmt.decl.ProgramUnit;

/**
 * Parser interface.
 * 
 * @author sccblom
 *
 */
public interface Parser {

  /**
   * Parse the given file and return the contents as a program unit.
   * 
   * @param file Name of the file to be parsed.
   * @return CompilationUnit representation of the contents of the file.
   */
  public ProgramUnit parse(File file);

}

