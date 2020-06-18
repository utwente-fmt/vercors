package vct.col.ast.stmt.decl;

/**
 * Enumeration of available specification formalisms.
 *  
 * @author sccblom
 *
 */
public enum SpecificationFormat {
  /** Sequential specification format.
   * Like JML, Boogie, Dafny, etc.
   */
  Sequential,
  /** Concurrent specifcation format.
   *  Like Chalice, Separation Logic, etc.
   *  This format is likely to be split into several others.
   *  E.g. Chalice for specs that can be sent to Chalice
   *  directly and Witness for specs that use a witness encoding.
   */
  Concurrent
}

