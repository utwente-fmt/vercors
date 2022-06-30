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
   * The default import set for generated files
   */
  val IMPORTS: List[Stat] = List(
    q"import vct.col.origin._",
    q"import vct.col.ref.Ref",
    q"import vct.col.resolve.Referrable",
    q"import vct.col.rewrite.ScopeContext",
    q"import vct.col.util.ExpectedError",
  )

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
    "SendDecl" -> q"sendScopes",
    "ParBlockDecl" -> q"parBlockScopes",
    "ParInvariantDecl" -> q"parInvariantScopes",
    "CLocalDeclaration" -> q"cLocalScopes",
    "CParam" -> q"cParams",
    "JavaLocalDeclaration" -> q"javaLocalScopes",
  )

  val DECLARATION_NAMESPACE: Map[String, Seq[Type.Name]] = Map(
    "GlobalDeclaration" -> Seq(t"Program"),
    "ClassDeclaration" -> Seq(t"Program"),
    "ADTDeclaration" -> Seq(t"Program"),
    "ModelDeclaration" -> Seq(t"Program"),
    "Variable" -> Seq(
      t"ParBlock", t"VecBlock", t"CatchClause", t"Scope", t"SignalsClause", // Explicit declarations
      t"AxiomaticDataType", t"JavaClass", t"JavaInterface", // Type arguments
      t"Predicate", t"InstancePredicate", // Arguments
      t"ModelProcess", t"ModelAction", t"ADTFunction",
      // given/yields variables must be deemed in the same scope as the applicable.
      // maybe they can be non-variables, since they are not referenced via Local?
      t"Program",
      // t"Function", t"Procedure",
      // t"InstanceFunction", t"InstanceMethod",
      // t"JavaConstructor", t"JavaMethod",
      // t"PVLConstructor",
      t"Forall", t"Starall", t"Exists", t"Sum", t"Product", t"Let", t"ScopedExpr" // Binders in expressions
    ),
    "LabelDecl" -> Seq(
      t"Function", t"Procedure",
      t"InstanceFunction", t"InstanceMethod",
      t"JavaConstructor", t"JavaMethod",
      t"PVLConstructor",
      // Potentially ParBlocks and other execution contexts (lambdas?) should be a scope too.
    ),
    "SendDecl" -> Seq(t"ParBlock"),
    "ParBlockDecl" -> Seq(t"ParBlock"),
    "ParInvariantDecl" -> Seq(t"ParInvariant"),
    "CLocalDeclaration" -> Seq(
      t"CGlobalDeclaration", t"CFunctionDefinition",
      t"Scope",
    ),
    "CParam" -> Seq(
      t"CGlobalDeclaration", t"CFunctionDefinition",
    ),
    "JavaLocalDeclaration" -> Seq(
      t"JavaConstructor", t"JavaMethod",
      t"Scope",
    ),
  )

  assert(DECLARATION_NAMESPACE.keys.toSet == DECLARATION_KINDS.keys.toSet)

  /**
   * The type that is the direct superclass of AbstractRewriter, and contains the default scopes mentioned above.
   */
  val PRE: Type.Name = Type.Name("Pre")
  val POST: Type.Name = Type.Name("Post")
  val SCOPE_CONTEXT: Type.Apply = Type.Apply(Type.Name("ScopeContext"), List(PRE, POST))

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
