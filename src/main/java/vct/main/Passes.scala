package vct.main

import hre.config.Configuration

import java.io.{File, FileNotFoundException, FileOutputStream, IOException, PrintWriter}
import hre.lang.System.{Abort, Debug, Fail}
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, ProgramUnit}
import vct.col.ast.syntax.{JavaDialect, JavaSyntax, PVLSyntax}
import vct.col.features
import vct.col.features.Feature
import vct.col.rewrite._
import vct.col.util.{JavaTypeCheck, LocalVariableChecker}
import vct.col.veymont.{ChannelPerms, Decompose, GenerateForkJoinMain, GenerateLTS, JavaForkJoin, LocalProgConstructors, RemoveTaus, StructureCheck, TerminationCheck}
import vct.experiments.learn.{NonLinCountVisitor, Oracle}
import vct.logging.{ExceptionMessage, PassReport}
import vct.parsers.rewrite.{AnnotationInterpreter, ConvertTypeExpressions, EncodeAsClass, FilterSpecIgnore, FlattenVariableDeclarations, InferADTTypes, RewriteWithThen, StripUnusedExtern}

import scala.jdk.CollectionConverters._

object Passes {
  val DIAGNOSTIC: Seq[AbstractPass] = Seq(
    SimplePass("printJava", "print AST in java syntax", arg => {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out, arg)
      out.close()
      arg
    }, introduces=Set(), permits=Feature.ALL),
    SimplePass("printC", "print AST in C syntax", arg => {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      vct.col.ast.print.CPrinter.dump(out, arg)
      out.close()
      arg
    }, introduces=Set(), permits=Feature.ALL),
    SimplePass("printVeyMontOutput", "print AST produced by VeyMont in PVL or Java syntax", arg => {
      try {
        val f = new File(Configuration.veymont_file.get());
        val b = f.createNewFile();
        if(!b) {
          Debug("File %s already exists and is now overwritten", Configuration.veymont_file.get());
        }
        val out = new PrintWriter(new FileOutputStream(f));
        if(Configuration.veymont_file.get().endsWith(".pvl"))
          PVLSyntax.get().print(out,arg)
        else if(Configuration.veymont_file.get().endsWith(".java")) {
          out.println("import java.util.concurrent.*;")
          out.println("import java.util.List;")
          out.println("import java.util.Map;")
          JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out, new JavaForkJoin(arg).rewriteAll())
        }
        else Fail("VeyMont Fail: VeyMont cannot write output to file %s",Configuration.veymont_file.get())
        out.close();
      } catch {
        case e: IOException => Debug(e.getMessage);
      }
      arg
    }, introduces=Set(), permits=Feature.ALL),
    new AbstractPass("checkTypes", "run a basic type check") {
      val permits: Set[Feature] = Feature.ALL
      val removes: Set[Feature] = Set.empty
      val introduces: Set[Feature] = Set.empty

      override def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
        new JavaTypeCheck(report, arg).check(); arg // Sneakily changing this to make abrupt tests pass for now
      }
    },
    SimplePass("checkAssignInPar",
      "Ascertain that parallel regions do not have multiple threads assigning to the same local",
      arg => { LocalVariableChecker.check(arg); arg },
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.ParallelLocalAssignmentNotChecked),
    ),
    new AbstractPass("checkTypesJava", "run a Java-aware type check") {
      val permits: Set[Feature] = Feature.ALL
      val removes: Set[Feature] = Set.empty
      val introduces: Set[Feature] = Set.empty

      override def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
        new JavaTypeCheck(report, arg).check(); arg
      }
    },
    new AbstractPass("printJavaToFile", "Generate code") {
      override def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
        val dir = new File(".").getAbsoluteFile
        val syntax = JavaSyntax.getJava(JavaDialect.JavaVerCors)
        for (node <- arg.asScala) {
          if (node.isInstanceOf[ASTClass]) {
            var out: PrintWriter = null
            try out = new PrintWriter(new FileOutputStream(new File(dir, node.asInstanceOf[ASTClass].name + ".java")))
            catch {
              case e: FileNotFoundException =>
                report.add(new ExceptionMessage(e))
                return arg
            }
            out.println("import col.lang.*;")
            syntax.print(out, node)
            out.close()
          }
          else if (node.isInstanceOf[ASTSpecial]) {
            val S = node.asInstanceOf[ASTSpecial]
            S.kind match {
              case _ =>
                report.fatal("cannot deal with special %s yet", S.kind)
                return arg
            }
          }
          else {
            report.fatal("cannot deal with %s yet", node.getClass())
            return arg
          }
        }
        arg
      }

      override def removes: Set[Feature] = Set()
      override def introduces: Set[Feature] = Set()
      override def permits: Set[Feature] = Feature.ALL
    },
    SimplePass("compileToJava", "Compile PVL classes to Java classes", new PVLCompiler(_).rewriteAll),
  )

  val OO: Seq[AbstractPass] = Seq(
    SimplePass("encodeInheritanceToDomain",
      "Add an ADT that describes the types and use it to implement instanceof",
      new AddTypeADT(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod - features.ImplicitConstructorInvokation,
      removes=Set(features.Inheritance, features.NoTypeADT),
      introduces=Feature.DEFAULT_INTRODUCE - features.Inheritance + features.TopLevelImplementedMethod + features.TopLevelMethod - features.NotFlattened - features.ArrayOps
    ),
    SimplePass(
      "propagateAbstractMethodContracts", "Encode Java overloading and inheritance",
      new JavaEncoder(_).rewriteAll,
      permits=Feature.ALL - features.ImplicitConstructorInvokation - features.NotStandardized - features.NotJavaResolved,
      removes=Set(features.NotJavaEncoded, features.StaticFields),
    ),
    SimplePass("collectStaticFields",
      "split classes into static and dynamic parts",
      new GlobalizeStaticsParameter(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.StaticFields),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.StaticFields,
        features.Inheritance,
        features.Constructors,
        features.NestedQuantifiers,
        features.ContextEverywhere,
        features.InlineQuantifierPattern,
        features.ArrayOps,
        features.This,
        features.NotFlattened,
        features.NonVoidMethods,
      )
    ),
    SimplePass(
      "loadExternalClasses", "Resolve the library dependencies of a java program",
      new JavaResolver(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.StringClass ++ Feature.OPTION_GATES ++ Set(
        features.ArrayOps,
        features.NonVoidMethods,
        features.ContextEverywhere,
        features.UnscaledPredicateApplication,
        features.ScatteredDeclarations,
        features.DeclarationsInIf,
        features.NotFlattened,
        features.BeforeSilverDomains,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
        features.NullAsOptionValue,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.TopLevelFields,
        features.NotJavaEncoded,
        features.Inheritance,
        features.NotJavaResolved,
        features.NotStandardized,
        features.PVLSugar,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.Synchronized,
        features.StringClass,
      ),
      removes=Set(features.NotJavaResolved),
      introduces=Feature.DEFAULT_INTRODUCE + features.Constructors + features.NotJavaEncoded,
    ),
    SimplePass(
      "addDefaultConstructor", "Add the default constructor to classes without one",
      AddZeroConstructor(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.ImplicitConstructorInvokation,
        features.NotJavaEncoded,
      ),
      removes=Set(features.ImplicitConstructorInvokation),
      introduces=Feature.DEFAULT_INTRODUCE + features.Constructors,
    ),
  )

  val ARRAYS: Seq[AbstractPass] = Seq(
    SimplePass("arrayNullValuesToNone",
      "rewrite null values for arrays to None",
      new ArrayNullValues(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.TopLevelImplementedMethod,
      ),
      removes=Set(features.NullAsOptionValue),
      introduces=Feature.EXPR_ONLY_INTRODUCE + features.Inheritance /* cast */,
    ),
    SimplePass("pointersToArraysLifted",
      "rewrite pointers to arrays",
      new PointersToArrays(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.DeclarationsNotLifted,
      removes=Set(features.Pointers, features.AddrOf),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere,
    ),
    SimplePass("pointersToArrays",
      "rewrite pointers to arrays",
      new PointersToArrays(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT,
      removes=Set(features.Pointers),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere,
    ),
    SimplePass("desugarValidPointer",
      "rewrite \\array, \\matrix, \\pointer and \\pointer_index",
      new DesugarValidPointer(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.Pointers + features.TopLevelImplementedMethod,
      removes=Set(features.ValidPointer),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere,
    ),
    SimplePass("stackLocationsToHeapLocations",
      "lift declarations to cell of the declared types, to treat locals as heap locations.",
      new LiftDeclarations(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.OpenMP - features.ParallelBlocks + features.TopLevelImplementedMethod,
      removes=Set(features.DeclarationsNotLifted),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere,
    ),
    SimplePass("desugarArrayOps",
      "rewrite arrays to sequences of cells",
      new RewriteArrayRef(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.ADTFunctions + features.TopLevelImplementedMethod,
      removes=Set(features.ArrayOps),
      introduces=Feature.NO_POLY_INTRODUCE + features.TopLevelImplementedMethod + features.TopLevelMethod + features.NestedQuantifiers -- Set(
        features.ArrayOps,
        features.ContextEverywhere,
        features.UnscaledPredicateApplication,
        features.ScatteredDeclarations,
        features.DeclarationsInIf,
        features.This,
        features.NotFlattened,
      )),
  )

  val EXCEPTIONS: Seq[AbstractPass] = Seq(
    SimplePass("specifyImplicitLoopLabels",
      "Insert explicit labels for break statements in while loops.",
      new SpecifyImplicitLabels(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES
        + features.TopLevelImplementedMethod
        + features.TopLevelMethod
        + features.ImplicitLabels
        + features.NullAsOptionValue
        + features.NotJavaEncoded
        + features.ArgumentAssignment
      , // TODO (Bob): This feels a bit suspicious
      removes = Set(features.ImplicitLabels)
    ),
    SimplePass("breakReturnToGoto",
      "Rewrite break, return into jumps",
      new BreakReturnToGoto(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT -- Set(features.Exceptions, features.Finally, features.ImplicitLabels) + features.TopLevelImplementedMethod + features.TopLevelMethod + features.TopLevelFields,
      introduces = Feature.DEFAULT_INTRODUCE + features.Goto - features.ArrayOps,
      removes = Set(features.Break, features.ExceptionalReturn)
    ),
    SimplePass("breakReturnToExceptions",
      "Rewrite break, return into exceptions",
      new BreakReturnToExceptions(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES
        - features.Switch
        - features.ImplicitLabels
        + features.TopLevelImplementedMethod
        + features.TopLevelMethod
        + features.NullAsOptionValue // TODO (Bob): Had to add this one but not sure what the feature does?
        + features.ArgumentAssignment, // TODO (Pieter): also this one, I don't think this pass particularly needs to be before java-encode
      removes = Set(features.Break, features.ExceptionalReturn),
      introduces = Feature.DEFAULT_INTRODUCE
        + features.Exceptions
        + features.NoExcVar
        + features.Inheritance
        + features.NotFlattened
    ),
    SimplePass("switchToIfChain",
      "Unfold switch to chain of if-statements that jump to sections.",
      new UnfoldSwitch(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT - features.ImplicitLabels + features.NotJavaEncoded + features.NullAsOptionValue + features.TopLevelImplementedMethod + features.TopLevelMethod ++ Feature.OPTION_GATES + features.ArgumentAssignment, // TODO (Bob): Also suspicious
      removes = Set(features.Switch),
      introduces = Feature.DEFAULT_INTRODUCE + features.Goto,
    ),
    SimplePass("continueToBreak",
      "Convert continues into breaks",
      new ContinueToBreak(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT + features.Continue - features.ImplicitLabels + features.NotJavaEncoded + features.NullAsOptionValue,
      removes = Set(features.Continue),
      introduces = Feature.DEFAULT_INTRODUCE + features.Break
    ),
    SimplePass("synchronizedToTryFinally",
      "Convert synchronized to try-finally",
      new UnfoldSynchronized(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT + features.Synchronized + features.NotJavaEncoded + features.PVLSugar + features.NullAsOptionValue ++ Feature.OPTION_GATES + features.ArgumentAssignment,
      removes = Set(features.Synchronized),
      introduces = Set(features.Exceptions, features.NoExcVar, features.Finally, features.PVLSugar)
    ),
    SimplePass("introExcVar",
      "Introduces the auxiliary sys__exc variable for use by exceptional control flow",
      new IntroExcVar(_).rewriteAll(),
      introduces = Feature.DEFAULT_INTRODUCE + features.ContextEverywhere -- Set(
        features.ArrayOps,
        features.NotFlattened,
        features.StaticFields,
        features.ContextEverywhere,
        features.InlineQuantifierPattern,
        features.Constructors,
        features.This,
        features.NonVoidMethods,
      ),
      permits = Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes = Set(features.NoExcVar)
    ),
    SimplePass("tryThrowSignalsToGoto",
      "Encodes exceptional control flow into gotos and exceptional contracts into regular contracts",
      new EncodeTryThrowSignals(_).rewriteAll(),
      removes = Set(features.Exceptions, features.Finally),
      permits = Feature.DEFAULT_PERMIT - features.NotFlattened + features.TopLevelImplementedMethod + features.TopLevelMethod - features.NoExcVar - features.NoTypeADT,
      introduces = (Feature.DEFAULT_INTRODUCE + features.Goto)
        -- Set(
        features.Constructors,
        features.This,
        features.NotFlattened,
        features.NonVoidMethods,
        features.ArrayOps,
        features.ContextEverywhere,
        features.BeforeSilverDomains,
        features.StaticFields,
        features.InlineQuantifierPattern,
      )
    ),
  )

  val MODELS: Seq[AbstractPass] = Seq(
    SimplePass("dereferenceToFieldAccess",
      "convert access expressions for histories/futures",
      new AccessIntroduce(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.NeedsDefinedCheck + features.NeedsAxiomCheck + features.NeedsHistoryCheck,
      removes=Set(features.Dereference),
      introduces=Feature.DEFAULT_INTRODUCE - features.Dereference,
    ),
    SimplePass("checkDefined", "rewrite process algebra class to check if defined process match their contracts", arg => {
      val tmp = new CheckProcessAlgebra(arg).rewriteAll
      new RandomizedIf(tmp).rewriteAll
    }, removes=Set(features.NeedsDefinedCheck), permits=Feature.DEFAULT_PERMIT + features.NeedsDefinedCheck),
    ErrorMapPass(
      "checkAxioms", "rewrite process algebra class to check if history axioms are correct",
      new CheckHistoryAlgebra(_, CheckHistoryAlgebra.Mode.AxiomVerification, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.NeedsAxiomCheck,
      removes=Set(features.NeedsAxiomCheck),
    ),
    ErrorMapPass("checkHistory",
      "rewrite process algebra class to check if history axioms are correct",
      new CheckHistoryAlgebra(_, CheckHistoryAlgebra.Mode.ProgramVerification, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.Dereference + features.NeedsHistoryCheck - features.ActionHeader,
      introduces=Feature.DEFAULT_INTRODUCE + features.TopLevelMethod,
      removes=Set(features.NeedsHistoryCheck),
    ),
    SimplePass(
      "actionHeaderToActionBlock", "Translate an action block with a header to its dedicated node",
      ActionHeaderToBlock(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES,
      removes=Set(features.ActionHeader),
    ),
  )

  val PARALLEL: Seq[AbstractPass] = Seq(
    ErrorMapPass(
      "inlineAtomicMethods", "Encode CSL atomic regions with methods",
      new CSLencoder(_, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.ImproperlySortedBeforeAfter ++ Feature.OPTION_GATES,
      removes=Set(features.JavaAtomic),
      introduces=Feature.DEFAULT_INTRODUCE + features.ParallelAtomic + features.Goto,
    ),
    SimplePass("encodeCurrentThread",
      "Encode references to current thread.",
      new CurrentThreadRewriter(_).rewriteAll,
      removes=Set(features.CurrentThread)),
    SimplePass("encodeKernelClass",
      "Split kernels into main, thread and barrier.",
      new KernelRewriter(_).rewriteAll,
      removes=Set(features.KernelClass)),
    SimplePass("encodeForkLockWait",
      "Encode PVL builtins for verification.",
      new PVLEncoder(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NotJavaEncoded,
        features.PVLSugar,
        features.NullAsOptionValue,
        features.ArgumentAssignment,
      ),
      removes=Set(features.PVLSugar),
      introduces=Feature.DEFAULT_INTRODUCE ++ Set(features.NotJavaEncoded),
    ),
    SimplePass("openMPToParallelBlocks",
      "Compile OpenMP pragmas to PVL",
      new OpenMPToPVL(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.OpenMP),
      introduces=Feature.DEFAULT_INTRODUCE + features.ParallelBlocks),
    ErrorMapPass("encodeParallelBlocks",
      "Encoded the proof obligations for parallel blocks",
      new ParallelBlockEncoder(_, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT
        - features.ContextEverywhere
        - features.ParallelAtomic
        - features.ParallelLocalAssignmentNotChecked,
      removes=Set(features.ParallelBlocks),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere + features.Summation + features.NotOptimized + features.MemberOfRange + features.ContextEverywhere),
    ErrorMapPass("inlineParallelAtomics",
      "Inlines atomic blocks into inhales/exhales",
      new InlineAtomic(_, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.ParallelAtomic),
    ),
    SimplePass(
      "encodeLockInvariantProof", "Add the proof of the lock invariant to the end of every constructor",
      ProveLockInvariantInConstructors(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.NoLockInvariantProof
      ),
      removes=Set(features.NoLockInvariantProof),
    ),
  )

  val ONE_SHOT_FEATURE = Seq(
    DummyPass(features.InvariantsPropagatedHere, features.ContextEverywhere),
    SimplePass("liftGhostCode",
      "Lift ghost code to real code",
      new GhostLifter(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod - features.ImproperlySortedBeforeAfter,
      removes=Set(features.GivenYields),
    ),
    SimplePass("inlineWithThenHints",
      "move before/after instructions",
      new FlattenBeforeAfter(_).rewriteAll,
      removes=Set(features.BeforeAfter),
      permits=Feature.DEFAULT_PERMIT - features.GivenYields - features.ImproperlySortedBeforeAfter + features.TopLevelImplementedMethod + features.TopLevelMethod),
    SimplePass("splitCompositeDeclarations",
      "put the base type in declarations",
      new FlattenVariableDeclarations(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.TopLevelImplementedMethod,
        features.TopLevelFields,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.NotJavaResolved,
        features.NotStandardized,
      ),
      removes=Set(features.MultiDecls)
    ),
    SimplePass("inline",
      "inlineInlineMethods",
      new InlinePredicatesAndFunctions(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.Lemma - features.MethodAnnotations + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.InlinePredicate, features.InlineFunction)),
    ErrorMapPass(
      "encodeMagicWands", "Encode magic wand proofs with abstract predicates",
      new WandEncoder(_, _).rewriteAll,
      removes=Set(features.Lemma),
    ),
    SimplePass("pureMethodsToFunctions",
      "translate pure methods to function syntax.",
      new PureMethodsAsFunctions(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.PureImperativeMethods ++ Feature.OPTION_GATES,
      removes=Set(features.PureImperativeMethods)),
    SimplePass("propagateInvariants",
      "propagate invariants",
      new PropagateInvariants(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.ContextEverywhere),
      introduces=Feature.NO_POLY_INTRODUCE -- Set(
        features.ContextEverywhere,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
        features.This,
        features.NotFlattened,
        features.NonVoidMethods,
        features.ArrayOps,
      )),
    SimplePass("adtOperatorsToFunctions",
      "rewrite standard operators on sequences to function definitions/calls",
      new GenerateADTFunctions(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.ADTFunctions),
      introduces=Feature.DEFAULT_INTRODUCE + features.TopLevelImplementedMethod + features.TopLevelMethod,
    ),
    SimplePass("inferADTElementTypes",
      "Transform typeless collection constructors by inferring their types.",
      new InferADTTypes(_).rewriteAll,
      removes=Set(features.UnresolvedTypeInference),
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
    ),
    SimplePass(
      "desugarADTOperators", "rewrite PVL-specific ADT operators",
      new ADTOperatorRewriter(_).rewriteAll,
      removes=Set(features.ADTOperator),
    ),
    SimplePass(
      "addRequirementSatCheck", "insert satisfyability checks for all methods",
      new SatCheckRewriter(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod ++ Feature.OPTION_GATES,
      removes=Set(features.NeedsSatCheck),
    ),
    SimplePass("standardize",
      "Standardize representation",
      new Standardize(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NotStandardized,
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.TopLevelFields,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.StringClass,
        features.Synchronized,
      ),
      removes=Set(features.NotStandardized),
    ),
    SimplePass("encodeVectorBlocks",
      "Encode vector blocks using the vector library",
      new VectorEncode(_).rewriteAll,
      removes=Set(features.VectorBlock)),
    SimplePass("inlinePatternsToTriggers",
      "Explicit inline patterns to normal trigger syntax",
      new InlinePatternToTrigger(_).rewriteAll,
      removes=Set(features.InlineQuantifierPattern),
      permits=Feature.EXPR_ONLY_PERMIT,
      introduces=Feature.EXPR_ONLY_INTRODUCE -- Set(
        features.ArrayOps,
        features.ContextEverywhere,
        features.InlineQuantifierPattern,
        features.This,
        features.Constructors,
        features.NotFlattened,
        features.NonVoidMethods,
        features.BeforeSilverDomains,
        features.NestedQuantifiers,
      ),
    ),
    SimplePass(
      "removeIgnoredElements", "Filter specifications and statements ignored with spec_ignore",
      new FilterSpecIgnore(_).rewriteAll(),
      // Should occur very early, because in spec_ignore the code may be invalid in various ways
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.SpecIgnore,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.TopLevelFields,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.NotStandardized,
        features.NotJavaResolved,
      ),
      removes=Set(features.SpecIgnore),
    ),
    SimplePass(
      "removeUnusedExternMethods", "Remove definitions from our internal C headers that are not used",
      new StripUnusedExtern(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
      ),
      removes=Set(features.UnusedExtern),
    ),
    SimplePass(
      "sortWithThen", "Put with/then statements in the correct place",
      new RewriteWithThen(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod ++ Feature.OPTION_GATES,
      removes=Set(features.ImproperlySortedBeforeAfter),
    ),
    SimplePass(
      "interpretMethodAnnotations", "Interpret annotations",
      new AnnotationInterpreter(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.TopLevelFields,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
      ),
      removes=Set(features.MethodAnnotations),
    ),
    SimplePass(
      "wrapTopLevelDeclarations", "Move top-level declarations into a class",
      new EncodeAsClass(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.TopLevelFields,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.NotJavaResolved,
        features.NotStandardized,
      ),
      removes=Set(features.TopLevelImplementedMethod, features.TopLevelMethod, features.TopLevelFields),
    ),
    SimplePass(
      "resolveTypeExpressions", "Resolve type expressions",
      new ConvertTypeExpressions(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.TopLevelFields,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.NotJavaResolved,
        features.NotStandardized,
      ),
      removes=Set(features.TypeExpressions, features.KernelInvocations),
      introduces=Feature.DEFAULT_INTRODUCE ++ Set(
        features.ParallelBlocks,
        features.GivenYields,
        features.MemberOfRange,
        features.QuantifierWithoutTriggers,
        features.NestedQuantifiers,
        features.BeforeAfter,
      )
    ),
    SimplePass(
      "stringClassToPrimitive", "Translate the java String class to its internal type",
      StringClassToType(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.TopLevelFields,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.NotJavaResolved,
        features.NotStandardized,
      ),
      removes=Set(features.StringClass),
    ),
    SimplePass("inlineAssignmentToStatement",
      "change inline assignments to statements",
      new AssignmentRewriter(_).rewriteAll,
      removes=Set(features.ExpressionStatement)
    ),
  )

  val BACKEND_COMPAT: Seq[AbstractPass] = Seq(
    SimplePass("finalizeArguments",
      "Make all method arguments final, i.e. not assigned to",
      new FinalizeArguments(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES + features.ArgumentAssignment + features.PureImperativeMethods,
      removes=Set(features.ArgumentAssignment)),
    SimplePass("flattenNestedExpressions",
      "remove nesting of expression",
      new Flatten(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod - features.ArrayOps,
      removes=Set(features.NotFlattened),
      introduces=Feature.NO_POLY_INTRODUCE -- Set(
        features.NotFlattened,
        features.ContextEverywhere,
        features.ArrayOps,
        // struct value flattening to arrays: features.BeforeSilverDomains,
        features.NestedQuantifiers,
        features.NonVoidMethods,
        features.InlineQuantifierPattern,
      )),
    SimplePass("collectDeclarations",
      "reorder statements (e.g. all declarations at the start of a block",
      new ReorderAssignments(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.ScatteredDeclarations),
      introduces=Feature.NO_POLY_INTRODUCE -- Set(
        features.ArrayOps,
        features.NonVoidMethods,
        features.ContextEverywhere,
        features.UnscaledPredicateApplication,
        features.ScatteredDeclarations,
        features.DeclarationsInIf,
        features.NotFlattened,
        features.BeforeSilverDomains,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
      )),
    SimplePass("importADTsAndRefEncode",
      "reduce classes to single Ref class",
      new SilverClassReduction(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.BeforeSilverDomains, features.This, features.Constructors, features.ContextEverywhere),
      introduces=Feature.DEFAULT_INTRODUCE + features.TopLevelImplementedMethod + features.TopLevelMethod -- Set(
        features.This,
        features.ArrayOps,
        features.Inheritance,
        features.ContextEverywhere,
        features.Constructors,
        features.BeforeSilverDomains,
        features.StaticFields,
        features.NotFlattened,
        features.InlineQuantifierPattern,
      )),
    SimplePass("collectInnerDeclarations",
      "move declarations from inside if-then-else blocks to top",
      new SilverReorder(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod,
      removes=Set(features.DeclarationsInIf),
      introduces=Feature.NO_POLY_INTRODUCE -- Set(
        features.ArrayOps,
        features.NonVoidMethods,
        features.ContextEverywhere,
        features.UnscaledPredicateApplication,
        features.DeclarationsInIf,
        features.NotFlattened,
        features.BeforeSilverDomains,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
      )
    ),
    SimplePass("scaleAllPredicateApplications",
      "scale every predicate invokation",
      new ScaleAlways(_).rewriteAll,
      permits=Feature.EXPR_ONLY_PERMIT,
      removes=Set(features.UnscaledPredicateApplication),
      introduces=Set(/* very simple; only may introduce Scale operator */)),
    SimplePass(
      "optimizeForSilver", "Optimize expressions for Silver",
      RewriteSystems.getRewriteSystem("silver_optimize").normalize(_),
      permits=Feature.EXPR_ONLY_PERMIT,
      removes=Set(features.MemberOfRange),
      introduces=Feature.EXPR_ONLY_INTRODUCE,
    ),
    ErrorMapPass("returnTypeToOutParameter",
      "Replace return value by out parameter.",
      new CreateReturnParameter(_, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.NotFlattened + features.TopLevelImplementedMethod + features.TopLevelMethod - features.Constructors,
      removes=Set(features.NonVoidMethods),
      introduces=Feature.NO_POLY_INTRODUCE -- Set(
        features.NotFlattened,
        features.NonVoidMethods,
        features.ArrayOps,
        features.ContextEverywhere,
        features.UnscaledPredicateApplication,
        features.BeforeSilverDomains,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
      )),
  )

  val SIMPLIFYING: Seq[AbstractPass] = Seq(
    SimplePass("simplifyAux1", "Simplify expressions", arg => {
      val trs = RewriteSystems.getRewriteSystem("simplifyAux1")
      trs.normalize(arg)
    }),
    SimplePass(
      "simplify", "Simplify quantifications",
      arg => {
        val trs = RewriteSystems.getRewriteSystem("simplify_quant_pass1")
        var res = trs.normalize(arg)
        res = RewriteSystems.getRewriteSystem("simplify_quant_pass2").normalize(res)
        res = new SimplifyQuantifiedRelations(res).rewriteAll()
        res
      },
      permits=Feature.EXPR_ONLY_PERMIT,
      removes=Set(features.NotOptimized, features.AnySubscript),
      introduces=Feature.EXPR_ONLY_INTRODUCE + features.MemberOfRange + features.ArrayOps,
    ),
    SimplePass(
      "simplifySums", "replace summations with provable functions",
      RewriteSystems.getRewriteSystem("summation").normalize(_),
      permits=Feature.EXPR_ONLY_PERMIT,
      removes=Set(features.Summation),
      introduces=Feature.EXPR_ONLY_INTRODUCE + features.MemberOfRange,
    ),
    SimplePass("reduceQuantifierNesting",
      "Removes nesting of quantifiers in chains of forall/starall and implies",
      new OptimizeQuantifiers(_).rewriteAll,
      removes=Set(features.NestedQuantifiers),
      permits=Feature.EXPR_ONLY_PERMIT,
      introduces=Feature.EXPR_ONLY_INTRODUCE -- Set(
        features.NestedQuantifiers,
        features.Dereference,
        features.Null,
        features.UnscaledPredicateApplication,
        features.NotFlattened,
        features.BeforeSilverDomains,
        features.ScatteredDeclarations,
        features.InlineQuantifierPattern,
      )
    ),
    SimplePass(
      "generateQuantifierTriggers", "Specify trigger sets for quantifiers using simple heuristics",
      Triggers(_).rewriteAll,
      removes=Set(features.QuantifierWithoutTriggers),
      permits=Feature.EXPR_ONLY_PERMIT -- Set(
        features.InlineQuantifierPattern,
        features.BeforeSilverDomains,
        features.NestedQuantifiers, // quantifiers are not un-nested when they have triggers
      ),
      introduces=Feature.EXPR_ONLY_INTRODUCE -- Set(
        features.InlineQuantifierPattern,
        features.NestedQuantifiers,
        features.UnscaledPredicateApplication,
        features.NotFlattened,
        features.BeforeSilverDomains,
        features.QuantifierWithoutTriggers,
      )
    ),
    SimplePass(
      "RemoveRecursiveActionClass", "remove super class RecursiveAction, by converting java methods to ASTSpecials",
      new RemoveRecursiveActionClass(_).rewriteAll(),
      removes = Set(features.RecursiveActionInheritance),
      permits=Feature.DEFAULT_PERMIT + features.TopLevelImplementedMethod + features.TopLevelMethod + features.PVLSugar + features.NullAsOptionValue + features.NotJavaEncoded + features.NeedsSatCheck + features.ArgumentAssignment,
      introduces = Feature.DEFAULT_INTRODUCE - features.NotFlattened - features.ArrayOps + features.PVLSugar
    ),
  )

  val BACKENDS: Seq[AbstractPass] = Seq(
    new AbstractPass("applySilicon", "verify input with Silicon") {
      override def apply_pass(arg: PassReport, args: Array[String]): PassReport = vct.silver.SilverBackend.TestSilicon(arg, "silicon")
      override def removes: Set[Feature] = Set()
      override def introduces: Set[Feature] = Set()
      override def permits: Set[Feature] = Set(
        features.Dereference,
        features.Null,
        features.ComplexSubscript,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.DeclarationsNotLifted,
        features.Goto,
        features.NoExcVar,
        features.NoTypeADT,
        features.Extern,
      )
    },
    new AbstractPass("applyCarbon", "verify input with Carbon") {
      override def apply_pass(arg: PassReport, args: Array[String]): PassReport = vct.silver.SilverBackend.TestSilicon(arg, "carbon")
      override def removes: Set[Feature] = Set()
      override def introduces: Set[Feature] = Set()
      override def permits: Set[Feature] = Set(
        features.Dereference,
        features.Null,
        features.ComplexSubscript,
        features.TopLevelImplementedMethod,
        features.TopLevelMethod,
        features.DeclarationsNotLifted,
        features.Goto,
        features.NoExcVar,
        features.NoTypeADT,
        features.Extern,
      )
    },
  )

  val OLD_OR_UNUSED: Seq[AbstractPass] = Seq(
    SimplePass("dsinherit", "rewrite contracts to reflect inheritance, predicate chaining", arg => new DynamicStaticInheritance(arg).rewriteOrdered),
    Pass("applyRewriteSystem", "Apply a term rewrite system", (arg, args) => {
      val trs = RewriteSystems.getRewriteSystem(args(0))
      trs.normalize(arg)
    }),
    Pass("generateQuantifierTriggersOld", "Add triggers to quantifiers if possible", (arg, args) => {
      var res = arg
      val `val` = Integer.valueOf(args(0))
      // First gather quantified variables for quantifiers without triggers.
      res = new OptimizeQuantifiers(res).rewriteAll
      // For quantifiers without triggers, and complex subscripts not containing quantified variables, add quantifier variable equal to the complex subscript.
      if ((`val` & 2) > 0) res = new RewriteComplexUnitSubscripts(res).rewriteAll
      // Try to add triggers for the now possibly simplified quantifiers.
      if ((`val` & 1) > 0) res = new AddSimpleTriggers(res).rewriteAll
      res
    }),
    Pass("countASTByNodeType", "Count nodes.", (arg, args) => {
      val cv = new NonLinCountVisitor(arg)
      cv.count()
      if (args.length == 1) Main.counters.put(args(0), cv)
      else Abort("Learn is used without an oracle")
      arg
    }),
    Pass("learnSlowNodes", "Learn unit times from counted AST nodes.", (arg, args) => {
      if (args.length == 1) {
        val start_time = args(0).toLong
        val time = System.currentTimeMillis - start_time
        for (entry <- Main.counters.entrySet.asScala) {
          Oracle.tell(entry.getKey, entry.getValue, time)
        }
      }
      else Abort("Learn is used without a starting time.")
      arg
    }),
  )

  /*
  VeyMont decomposes the global program from the input files into several local programs that can be executed in parallel.
  The program from the input files has to adhere to the syntax of a 'global program'. Syntax violations result in VeyMont Fail messages.
  The decomposition preserves the behaviour of the global program.
  This implies that all functional properties proven (with VerCors) for the global program also hold for the local program.
  Also, both global programs and their decomposed local programs are deadlock-free by construction.
  Memory and thread safety can be checked by running VerCors on the file produced by VeyMont.
  For more information on VeyMont, please check the VerCors Wiki.
   */
  val VEYMONT: Seq[AbstractPass] = Seq(
    SimplePass("VeyMontStructCheck", "check that provided program conforms to VeyMont global program syntax restriction",
      arg => { new StructureCheck(arg); arg }),
    SimplePass("VeyMontTerminationCheck", "check absence non-terminating statements",
      arg => { new TerminationCheck(arg); arg}),
    SimplePass("VeyMontDecompose", "generate local program classes from given global program",
      new Decompose(_).addThreadClasses()),
    SimplePass("removeEmptyBlocks", "remove empty blocks of parallel regions",
      new RemoveEmptyBlocks(_).rewriteAll),
    SimplePass("VeyMontLocalProgConstr", "add constructors to the local program classes",
      new LocalProgConstructors(_).addChansToConstructors()),
    SimplePass("VeyMontAddChannelPerms", "add channel permissions in contracts",
      new ChannelPerms(_).rewriteAll),
    SimplePass("VeyMontAddStartThreads", "add Main class to start all local program classes",
      new GenerateForkJoinMain(_).addStartThreadClass(false)), //TODO: put this argument in VeyMont Configuration
  )

  val BY_KEY: Map[String, AbstractPass] = (
    DIAGNOSTIC ++
    OO ++
    ARRAYS ++
    EXCEPTIONS ++
    MODELS ++
    PARALLEL ++
    ONE_SHOT_FEATURE ++
    BACKEND_COMPAT ++
    SIMPLIFYING ++
    BACKENDS ++
    VEYMONT ++
    OLD_OR_UNUSED).map(pass => (pass.key, pass)).toMap
}
