import scala.meta._

/**
 * Collection of all the constants needed to infer the structure of Col
 */
object ColDefs {
  /**
   * The package where all nodes live
   */
  val PACKAGE: Seq[String] = Seq("vct", "col", "ast")

  /**
   * The root type of declarations
   */
  val DECLARATION: String = "Declaration"
  val DECLARATION_TYPE: Type.Name = Type.Name(DECLARATION)

  /**
   * The different kinds of declaration that exist, as well as a mapping to their corresponding default scope.
   */
  val DECLARATION_KINDS: Map[String, Term.Name] = Map(
    "GlobalDeclaration" -> q"globalScopes",
    "ClassDeclaration" -> q"classScopes",
    "ADTDeclaration" -> q"adtScopes",
    "ModelDeclaration" -> q"modelScopes",
    "Variable" -> q"variableScopes",
    "LabelDecl" -> q"labelScopes",
    "ParBlockDecl" -> q"parBlockScopes",
    "ParInvariantDecl" -> q"parInvariantScopes",
    "CDeclaration" -> q"cLocalScopes",
    "CParam" -> q"cParams",
    "JavaLocalDeclaration" -> q"javaLocalScopes",
  )

  /**
   * The type that is the direct superclass of AbstractRewriter, and contains the default scopes mentioned above.
   */
  val SCOPE_CONTEXT: Type.Name = Type.Name("ScopeContext")

  /**
   * The fields that occupy blame and the origin of the node respectively.
   */
  val BLAME: String = "blame"
  val BLAME_TERM: Term.Name = Term.Name(BLAME)
  val BLAME_PAT: Pat.Var = Pat.Var(BLAME_TERM)
  val ORIGIN: String = "o"
  val ORIGIN_TERM: Term.Name = Term.Name(ORIGIN)
  val ORIGIN_PAT: Pat.Var = Pat.Var(ORIGIN_TERM)
}
