import scala.collection.mutable
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
    q"import vct.col.resolve.ctx.Referrable",
    q"import vct.col.origin.ExpectedError",
  )

  /**
   * The different kinds of declaration that exist, as well as a mapping to their corresponding default scope.
   */
  val DECLARATION_KINDS: Seq[String] = Seq(
    "GlobalDeclaration",
    "ClassDeclaration",
    "ADTDeclaration",
    "ModelDeclaration",
    "EnumConstant",
    "Variable",
    "LabelDecl",
    "SendDecl",
    "ParBlockDecl",
    "ParInvariantDecl",
    "CLocalDeclaration",
    "CParam",
    "JavaLocalDeclaration",
    "JavaParam",
    "BipIncomingData",
  )

  def scopes(kind: String): Term.Name =
    Term.Name(kind.charAt(0).toLower + kind.substring(1) + "s")

  val DECLARATION_NAMESPACE: mutable.ListMap[String, Seq[String]] = mutable.ListMap(
    "GlobalDeclaration" -> Seq("Program"),
    "ClassDeclaration" -> Seq("Program"),
    "ADTDeclaration" -> Seq("Program"),
    "ModelDeclaration" -> Seq("Program"),
    "EnumConstant" -> Seq("Program"),
    "Variable" -> Seq(
      "ParBlock", "VecBlock", "CatchClause", "Scope", "SignalsClause", // Explicit declarations
      "AxiomaticDataType", "JavaClass", "JavaInterface", // Type arguments
      "Predicate", "InstancePredicate", // Arguments
      "ModelProcess", "ModelAction", "ADTFunction",
      // given/yields variables must be deemed in the same scope as the applicable.
      // maybe they can be non-variables, since they are not referenced via Local?
      "Program",
      // "Function", "Procedure",
      // "InstanceFunction", "InstanceMethod",
      // "JavaConstructor", "JavaMethod",
      // "PVLConstructor",
      "Forall", "Starall", "Exists", "Sum", "Product", "Let", "ScopedExpr" // Binders in expressions
    ),
    "LabelDecl" -> Seq(
      "Function", "Procedure",
      "InstanceFunction", "InstanceMethod",
      "JavaConstructor", "JavaMethod",
      "PVLConstructor",
      // Potentially ParBlocks and other execution contexts (lambdas?) should be a scope too.
    ),
    "SendDecl" -> Seq("ParBlock", "Loop"),
    "ParBlockDecl" -> Seq("ParBlock"),
    "ParInvariantDecl" -> Seq("ParInvariant"),
    "CLocalDeclaration" -> Seq(
      "CGlobalDeclaration", "CFunctionDefinition",
      "Scope",
    ),
    "CParam" -> Seq(
      "CGlobalDeclaration", "CFunctionDefinition",
    ),
    "JavaLocalDeclaration" -> Seq(
      "JavaConstructor", "JavaMethod",
      "Scope",
    ),
    "BipIncomingData" -> Seq("BipTransition", "BipGuard"),
    "JavaParam" -> Seq("JavaMethod", "JavaAnnotationMethod", "JavaConstructor")
  )

  assert(DECLARATION_NAMESPACE.keys.toSet == DECLARATION_KINDS.toSet)
}
