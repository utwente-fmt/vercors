package vct.col.ast.syntax;

/**
 * The class enumerates the various dialects of Java specification languages.
 * @author Stefan Blom
 */
public enum JavaDialect {
  /**
   * Java with standard JML specifications.
   */
  JavaJML,
  /**
   * Java with the VerCors dialect of JML specifications.
   */
  JavaVerCors,
  /**
   * Java with VeriFast separation logic specifications.
   */
  JavaVeriFast
}
