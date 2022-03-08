package vct.main.passes

import hre.config.Configuration
import hre.lang.System.Debug
import vct.col.features.Feature
import vct.logging.PassReport
import vct.main.passes.Passes.BY_KEY

class SilverPassesGenerator extends PassesGeneratorTrait {

  def getPasses(report: PassReport): Seq[AbstractPass] = {
    val typeCheckReport = Passes.BY_KEY("checkTypesJava").apply_pass(report, Array())

    var features = Feature.scan(typeCheckReport.getOutput) ++ Set(
      // These are "gated" features: they are (too) hard to detect normally.
      vct.col.features.NotFlattened,
      vct.col.features.BeforeSilverDomains,
      vct.col.features.NullAsOptionValue,
      vct.col.features.NotOptimized,
      vct.col.features.DeclarationsNotLifted,
      vct.col.features.ParallelLocalAssignmentNotChecked,
      vct.col.features.NotJavaResolved,
      vct.col.features.InvariantsPropagatedHere,
    ) ++ Set(
      // These are normal features, but need to run always for some reason
      vct.col.features.ScatteredDeclarations, // this pass finds duplicate names (even if they're not scattered)
      vct.col.features.ImplicitLabels, // Can be detected, lazy, sorry
    )

    if(features.contains(vct.col.features.Extern))
    /* too hard to detect whether an extern decl is used, but it needs top-level-decls and we don't want to run that
     * for the silver frontend. */
      features += vct.col.features.UnusedExtern

    // options are encoded as gated features
    if(Configuration.currentConfiguration.satCheck.get()) features += vct.col.features.NeedsSatCheck
    if(Configuration.currentConfiguration.checkAxioms.get()) features += vct.col.features.NeedsAxiomCheck
    if(Configuration.currentConfiguration.checkDefined.get()) features += vct.col.features.NeedsDefinedCheck
    if(Configuration.currentConfiguration.checkHistory.get()) features += vct.col.features.NeedsHistoryCheck

    val passes = if (Configuration.currentConfiguration.stopAfterTypecheck.get()) {
      Seq()
    } else {
      computeGoal(features).get
    }

    if (Configuration.currentConfiguration.stopBeforeBackend.get()) {
      // We drop the last pass, which happens to be the silicon/carbon pass
      passes.init
    } else {
      passes
    }
  }

  def validChain(chain: Seq[AbstractPass], featuresIn: Set[Feature]): Boolean = {
    var features = featuresIn

    for(pass <- chain) {
      if((features -- pass.permits).nonEmpty) {
        Debug(s"Rejecting because ${pass.key} does not allow ${features -- pass.permits}")
        return false
      }

      features ++= pass.introduces
      features --= pass.removes
    }

    true
  }

  def minimize(chain: Seq[AbstractPass], featuresIn: Set[Feature]): Seq[AbstractPass] = {
    for(toRemove <- 0 until chain.size-1) {
      val newChain = chain.take(toRemove) ++ chain.drop(toRemove + 1)
      if(validChain(newChain, featuresIn)) {
        return minimize(newChain, featuresIn)
      }
    }
    chain
  }

  def filterNopPasses(chain: Seq[AbstractPass], featuresIn: Set[Feature]): Seq[AbstractPass] = {
    var features = featuresIn

    for(toRemove <- 0 until chain.size-1) {
      if(chain(toRemove).removes.intersect(features).isEmpty) {
        val newChain = chain.take(toRemove) ++ chain.drop(toRemove + 1)
        return filterNopPasses(newChain, featuresIn)
      }

      features ++= chain(toRemove).introduces
      features --= chain(toRemove).removes
    }

    chain
  }

  def computeGoal(featuresIn: Set[Feature]): Option[Seq[AbstractPass]] = {
    val toolPass = Configuration.currentConfiguration.silver.get() match {
      case "carbon" => BY_KEY("applyCarbon")
      case "silicon" => BY_KEY("applySilicon")
    }

    // Expand all choices
    val chains = ChainPart.inflate(silverPassOrder).map(_.map(BY_KEY(_)) :+ toolPass)

    // Filter out passes that don't remove anything (even before the chain is valid)
    val filteredChains = chains.map(filterNopPasses(_, featuresIn))

    // Filter for valid chains
    val validChains = filteredChains.filter(validChain(_, featuresIn))

    // Remove any passes that when removed constitute a valid chain
    val minimalChains = validChains.map(minimize(_, featuresIn))
    if(minimalChains.nonEmpty) {
      Some(minimalChains.minBy(_.size))
    } else {
      None
    }
  }

  object ChainPart {
    def inflate(parts: Seq[ChainPart]): Seq[Seq[String]] =
      parts.headOption match {
        case None => Seq(Seq())
        case Some(pass: Do) => inflate(parts.tail).map(pass.key +: _)
        case Some(Choose(alts@_* /* collect varargs into alts */)) =>
          val tail = inflate(parts.tail)
          alts.map(inflate).flatMap(branch => branch.flatMap(choice => tail.map(choice ++ _)))
      }
  }
  sealed trait ChainPart
  implicit class Do(val key: String) extends ChainPart
  case class Choose(choices: Seq[ChainPart]*) extends ChainPart

  val silverPassOrder: Seq[ChainPart] = Seq(
    "removeIgnoredElements",
    "splitCompositeDeclarations",
    "resolveTypeExpressions",
    "loadExternalClasses",
    "stringClassToPrimitive",
    "standardize",
    "interpretMethodAnnotations",
    "wrapTopLevelDeclarations",
    "removeUnusedExternMethods",
    "encodeLockInvariantProof",
    "synchronizedToTryFinally",
    "encodeForkLockWait",
    "specifyImplicitLoopLabels",
    "switchToIfChain",
    "addDefaultConstructor",
    "propagateAbstractMethodContracts",
    "arrayNullValuesToNone",
    "finalizeArguments",
    "actionHeaderToActionBlock",
    "addRequirementSatCheck",
    "pureMethodsToFunctions",
    "sortWithThen",
    Choose(
      Seq(),
      Seq("dereferenceToFieldAccess", "checkHistory"),
      Seq("dereferenceToFieldAccess", "checkAxioms"),
      Seq("dereferenceToFieldAccess", "checkDefined"),
    ),
    "desugarADTOperators",
    "inlineAssignmentToStatement",
    "continueToBreak",
    Choose(
      Seq("breakReturnToExceptions"),
      Seq("breakReturnToGoto"),
    ),
    "encodeCurrentThread",
    "collectStaticFields",
    "inferADTElementTypes",
    "encodeKernelClass",
    "checkAssignInPar",
    "encodeMagicWands",
    "inline",
    "inlineAtomicMethods",
    "openMPToParallelBlocks",
    "propagateInvariants",
    "dummy-InvariantsPropagatedHere",
    "compileToJava",
    "liftGhostCode",
    "inlineWithThenHints",
    "inlineParallelAtomics",
    "encodeParallelBlocks",
    Choose(
      Seq("stackLocationsToHeapLocations", "pointersToArraysLifted"),
      Seq("pointersToArrays"),
    ),
    "desugarValidPointer",
    "simplify",
    "simplifySums",
    "optimizeForSilver",
    "encodeVectorBlocks",
    "adtOperatorsToFunctions",
    "introExcVar",
    "desugarArrayOps",
    "flattenNestedExpressions",
    "encodeInheritanceToDomain",
    "tryThrowSignalsToGoto",
    "importADTsAndRefEncode",
    "returnTypeToOutParameter",
    "reduceQuantifierNesting",
    "inlinePatternsToTriggers",
    "generateQuantifierTriggers",
    "scaleAllPredicateApplications",
    "collectInnerDeclarations",
    "collectDeclarations",
  )
}
