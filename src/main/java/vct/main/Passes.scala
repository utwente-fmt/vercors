package vct.main

import java.io.{File, FileNotFoundException, FileOutputStream, PrintWriter}
import java.util

import hre.lang.System.Abort
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, ProgramUnit}
import vct.col.ast.syntax.{JavaDialect, JavaSyntax}
import vct.col.features
import vct.col.features.{Feature, RainbowVisitor}
import vct.col.rewrite._
import vct.col.util.{JavaTypeCheck, LocalVariableChecker, SimpleTypeCheck}
import vct.experiments.learn.{NonLinCountVisitor, Oracle}
import vct.logging.{ExceptionMessage, PassReport}
import vct.parsers.rewrite.{AnnotationInterpreter, ConvertTypeExpressions, EncodeAsClass, FilterSpecIgnore, FlattenVariableDeclarations, InferADTTypes, RewriteWithThen, StripUnusedExtern}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Passes {
  val BY_KEY: Map[String, AbstractPass] = Seq(
    SimplePass("java", "print AST in java syntax", arg => {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out, arg)
      out.close()
      arg
    }, introduces=Set(), permits=Feature.ALL),
    SimplePass("c", "print AST in C syntax", arg => {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      vct.col.ast.print.CPrinter.dump(out, arg)
      out.close()
      arg
    }, introduces=Set(), permits=Feature.ALL),
    SimplePass("add-type-adt",
      "Add an ADT that describes the types and use it to implement instanceof",
      new AddTypeADT(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations - features.ImplicitConstructorInvokation,
      removes=Set(features.Inheritance),
      introduces=Feature.DEFAULT_INTRODUCE - features.Inheritance + features.TopLevelDeclarations
    ),
    SimplePass("access",
      "convert access expressions for histories/futures",
      new AccessIntroduce(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.NeedsDefinedCheck + features.NeedsAxiomCheck + features.NeedsHistoryCheck,
      removes=Set(features.Dereference),
      introduces=Feature.DEFAULT_INTRODUCE - features.Dereference,
    ),
    SimplePass("assign", "change inline assignments to statements", new AssignmentRewriter(_).rewriteAll),
    new AbstractPass("silver", "verify input with Silver") {
      override def apply_pass(arg: PassReport, args: Array[String]): PassReport = vct.silver.SilverBackend.TestSilicon(arg, if(args.isEmpty) "silicon" else args(0))
      override def removes: Set[Feature] = Set()
      override def introduces: Set[Feature] = Set()
      override def permits: Set[Feature] = Set(
        features.Dereference,
        features.Null,
        features.ComplexSubscript,
        features.TopLevelDeclarations,
        features.DeclarationsNotLifted,
        features.Goto,
      )
    },
    new AbstractPass("check", "run a basic type check") {
      val permits: Set[Feature] = Feature.ALL
      val removes: Set[Feature] = Set.empty
      val introduces: Set[Feature] = Set.empty

      override def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
        new JavaTypeCheck(report, arg).check(); arg // Sneakily changing this to make abrupt tests pass for now
      }
    },
    SimplePass(
      "local-variable-check", "Ascertain that parallel regions do not have multiple threads assigning to the same local",
      arg => { LocalVariableChecker.check(arg); arg },
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
      removes=Set(features.ParallelLocalAssignmentNotChecked),
    ),
    SimplePass("array_null_values",
      "rewrite null values for arrays to None",
      new ArrayNullValues(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.TopLevelDeclarations,
      ),
      removes=Set(features.NullAsOptionValue),
      introduces=Feature.EXPR_ONLY_INTRODUCE,
    ),
    SimplePass("pointers_to_arrays_lifted",
      "rewrite pointers to arrays",
      new PointersToArrays(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.DeclarationsNotLifted,
      removes=Set(features.Pointers, features.AddrOf),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere,
    ),
    SimplePass("pointers_to_arrays",
      "rewrite pointers to arrays",
      new PointersToArrays(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT,
      removes=Set(features.Pointers),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere,
    ),
    SimplePass("desugar_valid_pointer",
      "rewrite \\array, \\matrix, \\pointer and \\pointer_index",
      new DesugarValidPointer(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.Pointers + features.TopLevelDeclarations,
      removes=Set(features.ValidPointer),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere,
    ),
    SimplePass("lift_declarations",
      "lift declarations to cell of the declared types, to treat locals as heap locations.",
      new LiftDeclarations(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.OpenMP - features.ParallelBlocks,
      removes=Set(features.DeclarationsNotLifted),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere,
    ),
    new AbstractPass("java-check", "run a Java-aware type check") {
      val permits: Set[Feature] = Feature.ALL
      val removes: Set[Feature] = Set.empty
      val introduces: Set[Feature] = Set.empty

      override def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
        new JavaTypeCheck(report, arg).check(); arg
      }
    },
    SimplePass("check-defined", "rewrite process algebra class to check if defined process match their contracts", arg => {
      val tmp = new CheckProcessAlgebra(arg).rewriteAll
      new RandomizedIf(tmp).rewriteAll
    }, removes=Set(features.NeedsDefinedCheck), permits=Feature.DEFAULT_PERMIT + features.NeedsDefinedCheck),
    ErrorMapPass(
      "check-axioms", "rewrite process algebra class to check if history axioms are correct",
      new CheckHistoryAlgebra(_, CheckHistoryAlgebra.Mode.AxiomVerification, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.NeedsAxiomCheck,
      removes=Set(features.NeedsAxiomCheck),
    ),
    ErrorMapPass("check-history",
      "rewrite process algebra class to check if history axioms are correct",
      new CheckHistoryAlgebra(_, CheckHistoryAlgebra.Mode.ProgramVerification, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.Dereference + features.NeedsHistoryCheck - features.ActionHeader,
      introduces=Feature.DEFAULT_INTRODUCE - features.Dereference,
      removes=Set(features.NeedsHistoryCheck),
    ),
    ErrorMapPass(
      "csl-encode", "Encode CSL atomic regions with methods",
      new CSLencoder(_, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.ImproperlySortedBeforeAfter,
      removes=Set(features.JavaAtomic),
      introduces=Feature.DEFAULT_INTRODUCE + features.ParallelAtomic,
    ),
// PB: obsolete?
//    SimplePass("class-conversion",
//      "Convert classes into records and procedures",
//      new ClassConversion(_).rewriteAll,
//      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
//      removes=Set(features.This, features.Constructors),
//      introduces=Feature.NO_POLY_INTRODUCE -- Set(
//        features.Arrays,
//        features.ContextEverywhere,
//        features.UnscaledPredicateApplication,
//        features.NestedQuantifiers,
//        features.InlineQuantifierPattern,
//      )),
    new AbstractPass("codegen", "Generate code") {
      override def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
        val dir = new File(args(0))
        if (dir.exists) if (!dir.isDirectory) {
          report.fatal("%s is not a directory", dir)
          return arg
        }
        else if (!dir.mkdirs) {
          report.fatal("could not create %s", dir)
          return arg
        }
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
    SimplePass("current_thread",
      "Encode references to current thread.",
      new CurrentThreadRewriter(_).rewriteAll,
      removes=Set(features.CurrentThread)),
    SimplePass(
      "java-encode", "Encode Java overloading and inheritance",
      new JavaEncoder(_).rewriteAll,
      permits=Feature.ALL - features.ImplicitConstructorInvokation - features.NotStandardized - features.NotJavaResolved,
      removes=Set(features.NotJavaEncoded, features.StaticFields),
    ),
    SimplePass("explicit_encoding", "encode required and ensured permission as ghost arguments", new ExplicitPermissionEncoding(_).rewriteAll),
    SimplePass("finalize_args",
      "Make all method arguments final, i.e. not assigned to",
      new FinalizeArguments(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES + features.ArgumentAssignment + features.PureImperativeMethods,
      removes=Set(features.ArgumentAssignment)),
    SimplePass("flatten",
      "remove nesting of expression",
      new Flatten(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations - features.ArrayOps,
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
    SimplePass("ghost-lift",
      "Lift ghost code to real code",
      new GhostLifter(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations - features.ImproperlySortedBeforeAfter,
      removes=Set(features.GivenYields),
    ),
    SimplePass("globalize",
      "split classes into static and dynamic parts",
      new GlobalizeStaticsParameter(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
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
      )),
    SimplePass("ds_inherit", "rewrite contracts to reflect inheritance, predicate chaining", arg => new DynamicStaticInheritance(arg).rewriteOrdered),
    SimplePass("flatten_before_after",
      "move before/after instructions",
      new FlattenBeforeAfter(_).rewriteAll,
      removes=Set(features.BeforeAfter),
      permits=Feature.DEFAULT_PERMIT - features.GivenYields - features.ImproperlySortedBeforeAfter),
    SimplePass("flatten_variable_declarations",
      "put the base type in declarations",
      new FlattenVariableDeclarations(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.TopLevelDeclarations,
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
      "Inline all methods marked as inline",
      new InlinePredicatesRewriter(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.Lemma - features.MethodAnnotations,
      removes=Set(features.InlinePredicate)),
    SimplePass("kernel-split",
      "Split kernels into main, thread and barrier.",
      new KernelRewriter(_).rewriteAll,
      removes=Set(features.KernelClass)),
    SimplePass("pvl-encode",
      "Encode PVL builtins for verification.",
      new PVLEncoder(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NotJavaEncoded,
        features.PVLSugar,
        features.NullAsOptionValue,
        features.ArgumentAssignment,
      ),
      removes=Set(features.PVLSugar),
      introduces=Set(features.NotJavaEncoded),
    ),
    ErrorMapPass(
      "magicwand", "Encode magic wand proofs with abstract predicates",
      new WandEncoder(_, _).rewriteAll,
      removes=Set(features.Lemma),
    ),
    SimplePass("modifies", "Derive modifies clauses for all contracts", arg => {
      new DeriveModifies().annotate(arg)
      arg
    }),
    SimplePass("openmp2pvl",
      "Compile OpenMP pragmas to PVL",
      new OpenMPToPVL(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
      removes=Set(features.OpenMP),
      introduces=Feature.DEFAULT_INTRODUCE + features.ParallelBlocks),
    ErrorMapPass("parallel_blocks",
      "Encoded the proof obligations for parallel blocks",
      new ParallelBlockEncoder(_, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT
        - features.ContextEverywhere
        - features.ParallelAtomic
        - features.ParallelLocalAssignmentNotChecked,
      removes=Set(features.ParallelBlocks),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere + features.Summation + features.NotOptimized),
    ErrorMapPass("inline-atomic",
      "Inlines atomic blocks into inhales/exhales",
      new InlineAtomic(_, _).rewriteAll,
      removes=Set(features.ParallelAtomic),
    ),
    SimplePass("pvl-compile", "Compile PVL classes to Java classes", new PVLCompiler(_).rewriteAll),
    SimplePass("reorder",
      "reorder statements (e.g. all declarations at the start of a block",
      new ReorderAssignments(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
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
    SimplePass("standardize-functions",
      "translate pure methods to function syntax.",
      new PureMethodsAsFunctions(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.PureImperativeMethods ++ Feature.OPTION_GATES,
      removes=Set(features.PureImperativeMethods)),
    SimplePass(
      "java_resolve", "Resolve the library dependencies of a java program",
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
        features.TopLevelDeclarations,
        features.NotJavaEncoded,
        features.Inheritance,
        features.NotJavaResolved,
        features.NotStandardized,
        features.PVLSugar,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.Synchronized,
      ),
      removes=Set(features.NotJavaResolved),
      introduces=Feature.DEFAULT_INTRODUCE + features.Constructors + features.NotJavaEncoded,
    ),
    SimplePass("propagate-invariants",
      "propagate invariants",
      new PropagateInvariants(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
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
    SimplePass("quant-optimize",
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
    Pass("rewrite", "Apply a term rewrite system", (arg, args) => {
      val trs = RewriteSystems.getRewriteSystem(args(0))
      trs.normalize(arg)
    }),
    SimplePass("rewrite_arrays",
      "rewrite arrays to sequences of cells",
      new RewriteArrayRef(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.ADTFunctions + features.TopLevelDeclarations,
      removes=Set(features.ArrayOps),
      introduces=Feature.NO_POLY_INTRODUCE /*+ features.TopLevelDeclarations*/ -- Set(
        features.ArrayOps,
        features.ContextEverywhere,
        features.UnscaledPredicateApplication,
        features.ScatteredDeclarations,
        features.DeclarationsInIf,
        features.This,
        features.NotFlattened,
      )),
    SimplePass("generate_adt_functions",
      "rewrite standard operators on sequences to function definitions/calls",
      new GenerateADTFunctions(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
      removes=Set(features.ADTFunctions),
      introduces=Feature.DEFAULT_INTRODUCE + features.TopLevelDeclarations,
    ),
    SimplePass("infer_adt_types",
      "Transform typeless collection constructors by inferring their types.",
      new InferADTTypes(_).rewriteAll,
      removes=Set(features.UnresolvedTypeInference),
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
    ),
    SimplePass(
      "adt_operator_rewrite", "rewrite PVL-specific ADT operators",
      new ADTOperatorRewriter(_).rewriteAll,
      removes=Set(features.ADTOperator),
    ),
    SimplePass("rm_cons", "???", new ConstructorRewriter(_).rewriteAll),
    SimplePass(
      "sat_check", "insert satisfyability checks for all methods",
      new SatCheckRewriter(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations ++ Feature.OPTION_GATES,
      removes=Set(features.NeedsSatCheck),
    ),
    SimplePass("silver-class-reduction",
      "reduce classes to single Ref class",
      new SilverClassReduction(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
      removes=Set(features.BeforeSilverDomains, features.This, features.Constructors),
      introduces=Feature.DEFAULT_INTRODUCE + features.TopLevelDeclarations -- Set(
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
    SimplePass("silver-reorder",
      "move declarations from inside if-then-else blocks to top",
      new SilverReorder(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.TopLevelDeclarations,
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
    SimplePass("scale-always",
      "scale every predicate invokation",
      new ScaleAlways(_).rewriteAll,
      permits=Feature.EXPR_ONLY_PERMIT,
      removes=Set(features.UnscaledPredicateApplication),
      introduces=Set(/* very simple; only may introduce Scale operator */)),
    SimplePass(
      "silver-optimize", "Optimize expressions for Silver",
      RewriteSystems.getRewriteSystem("silver_optimize").normalize(_),
      permits=Feature.EXPR_ONLY_PERMIT,
      removes=Set(features.MemberOfRange),
      introduces=Feature.EXPR_ONLY_INTRODUCE,
    ),
    SimplePass("chalice-optimize", "Optimize expressions for Chalice", arg => {
      val trs = RewriteSystems.getRewriteSystem("chalice_optimize")
      trs.normalize(arg)
    }),
    SimplePass("simplify_expr", "Simplify expressions", arg => {
      val trs = RewriteSystems.getRewriteSystem("simplify_expr")
      trs.normalize(arg)
    }),
    SimplePass(
      "simplify_quant", "Simplify quantifications",
      arg => {
        val trs = RewriteSystems.getRewriteSystem("simplify_quant_pass1")
        var res = trs.normalize(arg)
        res = RewriteSystems.getRewriteSystem("simplify_quant_pass2").normalize(res)
        res
      },
      permits=Feature.EXPR_ONLY_PERMIT,
      removes=Set(features.NotOptimized, features.AnySubscript),
      introduces=Feature.EXPR_ONLY_INTRODUCE + features.MemberOfRange + features.ArrayOps,
    ),
    SimplePass(
      "simplify_sums", "replace summations with provable functions",
      RewriteSystems.getRewriteSystem("summation").normalize(_),
      permits=Feature.EXPR_ONLY_PERMIT,
      removes=Set(features.Summation),
      introduces=Feature.EXPR_ONLY_INTRODUCE + features.MemberOfRange,
    ),
    SimplePass("simplify_quant_relations", "simplify quantified relational expressions", new SimplifyQuantifiedRelations(_).rewriteAll),
    SimplePass("standardize",
      "Standardize representation",
      new Standardize(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NotStandardized,
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.TopLevelDeclarations,
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
    /*SimplePass("strip_constructors",
      "Strip constructors from classes",
      new StripConstructors(_).rewriteAll,
      removes=Set(features.Constructors),
      introduces=Set(/* does what it says on the tin */)),*/
    ErrorMapPass("create-return-parameter",
      "Replace return value by out parameter.",
      new CreateReturnParameter(_, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.NotFlattened + features.TopLevelDeclarations - features.Constructors,
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
    SimplePass("vector-encode",
      "Encode vector blocks using the vector library",
      new VectorEncode(_).rewriteAll,
      removes=Set(features.VectorBlock)),
    SimplePass("chalice-preprocess", "Pre processing for chalice", new ChalicePreProcess(_).rewriteAll),
    Pass("simple_triggers", "Add triggers to quantifiers if possible", (arg, args) => {
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
    Pass("count", "Count nodes.", (arg, args) => {
      val cv = new NonLinCountVisitor(arg)
      cv.count()
      if (args.length == 1) Main.counters.put(args(0), cv)
      else Abort("Learn is used without an oracle")
      arg
    }),
    Pass("learn", "Learn unit times from counted AST nodes.", (arg, args) => {
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
    SimplePass("specify-implicit-labels",
      "Insert explicit labels for break statements in while loops.",
      new SpecifyImplicitLabels(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES
        + features.TopLevelDeclarations
        + features.ImplicitLabels
        + features.NullAsOptionValue
        + features.NotJavaEncoded
      , // TODO (Bob): This feels a bit suspicious
      removes = Set(features.ImplicitLabels)
    ),
    SimplePass("break-return-to-goto",
      "Rewrite break, return into jumps",
      new BreakReturnToGoto(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT -- Set(features.Exceptions, features.Finally, features.ImplicitLabels) + features.TopLevelDeclarations,
      introduces = Feature.DEFAULT_INTRODUCE + features.Goto - features.ArrayOps,
      removes = Set(features.Break, features.ExceptionalReturn)
    ),
    SimplePass("break-return-to-exceptions",
      "Rewrite break, return into exceptions",
      new BreakReturnToExceptions(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES
        - features.Switch
        - features.ImplicitLabels
        + features.TopLevelDeclarations
        + features.NotJavaEncoded
        + features.NullAsOptionValue // TODO (Bob): Had to add this one but not sure what the feature does?
        + features.ArgumentAssignment, // TODO (Pieter): also this one, I don't think this pass particularly needs to be before java-encode
      removes = Set(features.Break, features.ExceptionalReturn),
      introduces = Feature.DEFAULT_INTRODUCE
        + features.Exceptions
        + features.Inheritance
        + features.NotFlattened
        + features.NotJavaEncoded
    ),
    SimplePass("unfold-switch",
      "Unfold switch to chain of if-statements that jump to sections.",
      new UnfoldSwitch(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT - features.ImplicitLabels + features.NotJavaEncoded + features.NullAsOptionValue + features.TopLevelDeclarations ++ Feature.OPTION_GATES, // TODO (Bob): Also suspicious
      removes = Set(features.Switch)
    ),
    SimplePass("continue-to-break",
      "Convert continues into breaks",
      new ContinueToBreak(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT + features.Continue - features.ImplicitLabels + features.NotJavaEncoded + features.NullAsOptionValue,
      removes = Set(features.Continue),
      introduces = Feature.DEFAULT_INTRODUCE + features.Break
    ),
    SimplePass("unfold-synchronized",
      "Convert synchronized to try-finally",
      new UnfoldSynchronized(_).rewriteAll(),
      permits = Feature.DEFAULT_PERMIT + features.Synchronized + features.NotJavaEncoded + features.PVLSugar + features.NullAsOptionValue ++ Feature.OPTION_GATES,
      removes = Set(features.Synchronized),
      introduces = Set(features.Exceptions, features.Finally, features.PVLSugar)
    ),
    SimplePass("intro-exc-var",
      "Introduces the auxiliary sys__exc variable for use by exceptional control flow",
      new IntroExcVar(_).rewriteAll(),
      introduces = Feature.DEFAULT_INTRODUCE ++ Set(features.ExcVar, features.ContextEverywhere) -- Set(
        features.ArrayOps,
        features.NotFlattened,
        features.StaticFields,
        features.ContextEverywhere,
        features.InlineQuantifierPattern,
        features.Constructors,
        features.This,
        features.NonVoidMethods,
      ),
      permits = Feature.DEFAULT_PERMIT - features.Inheritance + features.TopLevelDeclarations,
      removes = Set(features.Exceptions, features.Finally) // TODO (Bob): This is kind of lying...
    ),
    SimplePass("encode-try-throw-signals",
      "Encodes exceptional control flow into gotos and exceptional contracts into regular contracts",
      new EncodeTryThrowSignals(_).rewriteAll(),
      removes = Set(features.ExcVar),
      permits = Feature.DEFAULT_PERMIT + features.ExcVar - features.NotFlattened + features.TopLevelDeclarations,
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
    SimplePass(
      "gen-triggers", "Specify trigger sets for quantifiers using simple heuristics",
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
    SimplePass("inline-pattern-to-trigger",
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
      ),
    ),
    SimplePass(
      "spec-ignore", "Filter specifications and statements ignored with spec_ignore",
      new FilterSpecIgnore(_).rewriteAll(),
      // Should occur very early, because in spec_ignore the code may be invalid in various ways
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.SpecIgnore,
        features.TopLevelDeclarations,
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
      "unused-extern", "Remove definitions from our internal C headers that are not used",
      new StripUnusedExtern(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.TopLevelDeclarations,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
      ),
      removes=Set(features.UnusedExtern),
    ),
    SimplePass(
      "sort-before-after", "Put with/then statements in the correct place",
      new RewriteWithThen(_).rewriteAll(),
      removes=Set(features.ImproperlySortedBeforeAfter),
    ),
    SimplePass(
      "interpret-annotations", "Interpret annotations",
      new AnnotationInterpreter(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.TopLevelDeclarations,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
      ),
      removes=Set(features.MethodAnnotations),
    ),
    SimplePass(
      "top-level-decls", "Move top-level declarations into a class",
      new EncodeAsClass(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.TopLevelDeclarations,
        features.TopLevelFields,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.NotJavaResolved,
        features.NotStandardized,
      ),
      removes=Set(features.TopLevelDeclarations, features.TopLevelFields),
    ),
    SimplePass(
      "type-expressions", "Resolve type expressions",
      new ConvertTypeExpressions(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.TopLevelDeclarations,
        features.TopLevelFields,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.NotJavaResolved,
        features.NotStandardized,
      ),
      removes=Set(features.TypeExpressions),
    ),
    SimplePass(
      "zero-constructor", "Add the default constructor to classes without one",
      AddZeroConstructor(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.TopLevelDeclarations,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.ImplicitConstructorInvokation,
        features.NotJavaEncoded,
      ),
      removes=Set(features.ImplicitConstructorInvokation),
      introduces=Feature.DEFAULT_INTRODUCE + features.Constructors,
    ),
    SimplePass(
      "lock-invariant-proof", "Add the proof of the lock invariant to the end of every constructor",
      ProveLockInvariantInConstructors(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.NullAsOptionValue,
        features.TopLevelDeclarations,
        features.ArgumentAssignment,
        features.PureImperativeMethods,
        features.PVLSugar,
        features.NotJavaEncoded,
        features.LockInvariant
      ),
      removes=Set(features.LockInvariant),
    ),
    SimplePass(
      "string-class", "Translate the java String class to its internal type",
      StringClassToType(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES ++ Set(
        features.TypeExpressions,
        features.NullAsOptionValue,
        features.TopLevelDeclarations,
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
    SimplePass(
      "action-header", "Translate an action block with a header to its dedicated node",
      ActionHeaderToBlock(_).rewriteAll(),
      permits=Feature.DEFAULT_PERMIT ++ Feature.OPTION_GATES,
      removes=Set(features.ActionHeader),
    )
  ).map(_.tup).toMap
}