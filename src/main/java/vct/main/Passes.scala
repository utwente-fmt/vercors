package vct.main

import java.io.{File, FileNotFoundException, FileOutputStream, PrintWriter}
import java.util

import hre.lang.System.Abort
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, ProgramUnit}
import vct.col.ast.syntax.{JavaDialect, JavaSyntax}
import vct.col.features
import vct.col.features.{Feature, RainbowVisitor}
import vct.col.rewrite._
import vct.col.util.{JavaTypeCheck, SimpleTypeCheck}
import vct.experiments.learn.{NonLinCountVisitor, Oracle}
import vct.logging.{ExceptionMessage, PassReport}
import vct.parsers.rewrite.{FlattenVariableDeclarations, InferADTTypes}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Passes {
  val BY_KEY: Map[String, AbstractPass] = Map(
    "java" -> SimplePass("print AST in java syntax", arg => {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out, arg)
      out.close()
      arg
    }, introduces=Set(), permits=Feature.ALL),
    "c" -> SimplePass("print AST in C syntax", arg => {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      vct.col.ast.print.CPrinter.dump(out, arg)
      out.close()
      arg
    }, introduces=Set(), permits=Feature.ALL),
    "add-type-adt" -> SimplePass(
      "Add an ADT that describes the types and use it to implement instanceof",
      new AddTypeADT(_).rewriteAll,
      removes=Set(features.Inheritance),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.Inheritance
      )),
    "access" -> SimplePass(
      "convert access expressions for histories/futures",
      new AccessIntroduce(_).rewriteAll,
      removes=Set(features.Dereference),
      introduces=Feature.DEFAULT_INTRODUCE - features.Dereference,
    ),
    "assign" -> SimplePass("change inline assignments to statements", new AssignmentRewriter(_).rewriteAll),
    "silver" -> new AbstractPass("verify input with Silver") {
      override def apply_pass(arg: PassReport, args: Array[String]): PassReport = vct.silver.SilverBackend.TestSilicon(arg, if(args.isEmpty) "silicon" else args(0))
      override def removes: Set[Feature] = Set()
      override def introduces: Set[Feature] = Set()
      override def permits: Set[Feature] = Set(features.Dereference, features.Null, features.ComplexSubscript)
    },
    "check" -> new AbstractPass("run a basic type check") {
      val permits: Set[Feature] = Feature.ALL
      val removes: Set[Feature] = Set.empty
      val introduces: Set[Feature] = Set.empty

      override def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
        new SimpleTypeCheck(report, arg).check(); arg
      }
    },
    "array_null_values" -> SimplePass(
      "rewrite null values for arrays to None",
      new ArrayNullValues(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT ++ Set(
        features.NullAsOptionValue,
        features.ArgumentAssignment,
      ),
      removes=Set(features.NullAsOptionValue)),
    "pointers_to_arrays" -> SimplePass(
      "rewrite pointers to arrays",
      new PointersToArrays(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.AddrOf,
      removes=Set(features.Pointers)),
    "desugar_valid_pointer" -> SimplePass(
      "rewrite \\array, \\matrix, \\pointer and \\pointer_index",
      new DesugarValidPointer(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.Pointers,
      removes=Set(features.ValidPointer)),
    "lift_declarations" -> SimplePass(
      "lift declarations to cell of the declared types, to treat locals as heap locations.",
      new LiftDeclarations(_).rewriteAll,
      removes=Set(features.AddrOf)),
    "java-check" -> new AbstractPass("run a Java-aware type check") {
      val permits: Set[Feature] = Feature.ALL
      val removes: Set[Feature] = Set.empty
      val introduces: Set[Feature] = Set.empty

      override def apply(report: PassReport, arg: ProgramUnit, args: Array[String]): ProgramUnit = {
        new JavaTypeCheck(report, arg).check(); arg
      }
    },
    "check-defined" -> SimplePass("rewrite process algebra class to check if defined process match their contracts", arg => {
      val tmp = new CheckProcessAlgebra(arg).rewriteAll
      new RandomizedIf(tmp).rewriteAll
    }),
    "check-axioms" -> ErrorMapPass("rewrite process algebra class to check if history axioms are correct", new CheckHistoryAlgebra(_, CheckHistoryAlgebra.Mode.AxiomVerification, _).rewriteAll),
    "check-history" -> ErrorMapPass(
      "rewrite process algebra class to check if history axioms are correct",
      new CheckHistoryAlgebra(_, CheckHistoryAlgebra.Mode.ProgramVerification, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.Dereference
    ),
    "csl-encode" -> new ErrorMapPass(
      "Encode CSL atomic regions with methods",
      new CSLencoder(_, _).rewriteAll,
      removes=Set(features.JavaAtomic)),
    "class-conversion" -> SimplePass(
      "Convert classes into records and procedures",
      new ClassConversion(_).rewriteAll,
      removes=Set(features.This, features.Constructors),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.This,
        features.Arrays,
        features.Inheritance,
        features.ContextEverywhere,
        features.Constructors,
        features.UnscaledPredicateApplication,
        features.StaticFields,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
      )),
    "codegen" -> new AbstractPass("Generate code") {
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
              case ASTSpecial.Kind.Comment =>
              // TODO keep comments.
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
    "current_thread" -> SimplePass(
      "Encode references to current thread.",
      new CurrentThreadRewriter(_).rewriteAll,
      removes=Set(features.CurrentThread)),
    "java-encode" -> SimplePass("Encode Java overloading and inheritance", new JavaEncoder(_).rewriteAll),
    "explicit_encoding" -> SimplePass("encode required and ensured permission as ghost arguments", new ExplicitPermissionEncoding(_).rewriteAll),
    "finalize_args" -> SimplePass(
      "Make all method arguments final, i.e. not assigned to",
      new FinalizeArguments(_).rewriteAll,
      permits=Feature.DEFAULT_PERMIT + features.ArgumentAssignment,
      removes=Set(features.ArgumentAssignment)),
    "flatten" -> SimplePass(
      "remove nesting of expression",
      new Flatten(_).rewriteAll,
      removes=Set(features.NotFlattened),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.NotFlattened,
        features.Inheritance,
        features.Constructors,
        features.ContextEverywhere,
        features.StaticFields,
        features.This,
        features.Arrays,
        // struct value flattening to arrays: features.BeforeSilverDomains,
        features.NestedQuantifiers,
        features.NonVoidMethods,
        features.InlineQuantifierPattern,
      )),
    "ghost-lift" -> SimplePass(
      "Lift ghost code to real code",
      new GhostLifter(_).rewriteAll,
      removes=Set(features.GivenYields)),
    "globalize" -> SimplePass(
      "split classes into static and dynamic parts",
      new GlobalizeStaticsParameter(_).rewriteAll,
      removes=Set(features.StaticFields),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.StaticFields,
        features.Inheritance,
        features.Constructors,
        features.NestedQuantifiers,
        features.ContextEverywhere,
        features.InlineQuantifierPattern,
      )),
    "ds_inherit" -> SimplePass("rewrite contracts to reflect inheritance, predicate chaining", arg => new DynamicStaticInheritance(arg).rewriteOrdered),
    "flatten_before_after" -> SimplePass(
      "move before/after instructions",
      new FlattenBeforeAfter(_).rewriteAll,
      removes=Set(features.BeforeAfter),
      permits=Feature.DEFAULT_PERMIT - features.GivenYields),
    "flatten_variable_declarations" -> SimplePass(
      "put the base type in declarations",
      new FlattenVariableDeclarations(_).rewriteAll,
      removes=Set(features.MultiDecls)),
    "inline" -> SimplePass(
      "Inline all methods marked as inline",
      new InlinePredicatesRewriter(_).rewriteAll,
      removes=Set(features.InlinePredicate)),
    "kernel-split" -> SimplePass(
      "Split kernels into main, thread and barrier.",
      new KernelRewriter(_).rewriteAll,
      removes=Set(features.KernelClass)),
    "pvl-encode" -> SimplePass(
      "Encode PVL builtins for verification.",
      new PVLEncoder(_).rewriteAll,
      removes=Set(features.PVLSugar)),
    "magicwand" -> ErrorMapPass("Encode magic wand proofs with abstract predicates", new WandEncoder(_, _).rewriteAll),
    "modifies" -> SimplePass("Derive modifies clauses for all contracts", arg => {
      new DeriveModifies().annotate(arg)
      arg
    }),
    "openmp2pvl" -> SimplePass(
      "Compile OpenMP pragmas to PVL",
      new OpenMPToPVL(_).rewriteAll,
      removes=Set(features.OpenMP),
      introduces=Feature.DEFAULT_INTRODUCE + features.ParallelBlocks),
    "parallel_blocks" -> ErrorMapPass(
      "Encoded the proof obligations for parallel blocks",
      new ParallelBlockEncoder(_, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.ContextEverywhere - features.ParallelAtomic,
      removes=Set(features.ParallelBlocks),
      introduces=Feature.DEFAULT_INTRODUCE - features.ContextEverywhere),
    "inline-atomic" -> ErrorMapPass(
      "Inlines atomic blocks into inhales/exhales",
      new InlineAtomic(_, _).rewriteAll,
      removes=Set(features.ParallelAtomic),
    ),
    "pvl-compile" -> SimplePass("Compile PVL classes to Java classes", new PVLCompiler(_).rewriteAll),
    "reorder" -> SimplePass(
      "reorder statements (e.g. all declarations at the start of a block",
      new ReorderAssignments(_).rewriteAll,
      removes=Set(features.ScatteredDeclarations),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.This,
        features.Arrays,
        features.NonVoidMethods,
        features.Inheritance,
        features.ContextEverywhere,
        features.StaticFields,
        features.Constructors,
        features.UnscaledPredicateApplication,
        features.ScatteredDeclarations,
        features.DeclarationsInIf,
        features.NotFlattened,
        features.BeforeSilverDomains,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
      )),
    "standardize-functions" -> SimplePass(
      "translate pure methods to function syntax.",
      new PureMethodsAsFunctions(_).rewriteAll,
      removes=Set(features.PureImperativeMethods)),
    "java_resolve" -> SimplePass("Resolve the library dependencies of a java program", new JavaResolver(_).rewriteAll),
    "propagate-invariants" -> SimplePass(
      "propagate invariants",
      new PropagateInvariants(_).rewriteAll,
      removes=Set(features.ContextEverywhere),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.ContextEverywhere,
        features.Inheritance,
        features.Constructors,
        features.StaticFields,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
      )),
    "quant-optimize" -> SimplePass(
      "Removes nesting of quantifiers in chains of forall/starall and implies",
      new OptimizeQuantifiers(_).rewriteAll,
      removes=Set(features.NestedQuantifiers),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.NestedQuantifiers,
        features.This,
        features.Arrays,
        features.Dereference,
        features.NonVoidMethods,
        features.ContextEverywhere,
        features.StaticFields,
        features.Constructors,
        features.Null,
        features.UnscaledPredicateApplication,
        features.NotFlattened,
        features.BeforeSilverDomains,
        features.ScatteredDeclarations,
        features.DeclarationsInIf,
        features.InlineQuantifierPattern,
      )
    ),
    "rewrite" -> Pass("Apply a term rewrite system", (arg, args) => {
      val trs = RewriteSystems.getRewriteSystem(args(0))
      trs.normalize(arg)
    }),
    "rewrite_arrays" -> SimplePass(
      "rewrite arrays to sequences of cells",
      new RewriteArrayRef(_).rewriteAll,
      removes=Set(features.Arrays),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.Arrays,
        features.Inheritance,
        features.ContextEverywhere,
        features.StaticFields,
        features.Constructors,
        features.UnscaledPredicateApplication,
        features.ScatteredDeclarations,
        features.DeclarationsInIf,
      )),
    "generate_adt_functions" -> SimplePass(
      "rewrite  standard operators on sequences to function definitions/calls",
      new GenerateADTFunctions(_).rewriteAll,
      removes=Set(features.ADTOperators)),
    "infer_adt_types" -> SimplePass(
      "Transform typeless collection constructors by inferring their types.",
      new InferADTTypes(_).rewriteAll,
      removes=Set(features.UnresolvedTypeInference)),
    "adt_operator_rewrite" -> SimplePass("rewrite PVL-specific ADT operators", new ADTOperatorRewriter(_).rewriteAll),
    "rm_cons" -> SimplePass("???", new ConstructorRewriter(_).rewriteAll),
    "sat_check" -> SimplePass("insert satisfyability checks for all methods", new SatCheckRewriter(_).rewriteAll),
    "silver-class-reduction" -> SimplePass(
      "reduce classes to single Ref class",
      new SilverClassReduction(_).rewriteAll,
      removes=Set(features.BeforeSilverDomains),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.This,
        features.Arrays,
        features.Inheritance,
        features.ContextEverywhere,
        features.Constructors,
        features.BeforeSilverDomains,
        features.StaticFields,
        features.NonVoidMethods,
        features.NotFlattened,
        features.InlineQuantifierPattern,
      )),
    "silver-reorder" -> SimplePass(
      "move declarations from inside if-then-else blocks to top",
      new SilverReorder(_).rewriteAll,
      removes=Set(features.DeclarationsInIf),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.This,
        features.Arrays,
        features.NonVoidMethods,
        features.Inheritance,
        features.ContextEverywhere,
        features.StaticFields,
        features.Constructors,
        features.UnscaledPredicateApplication,
        features.DeclarationsInIf,
        features.NotFlattened,
        features.BeforeSilverDomains,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
      )
    ),
    "scale-always" -> SimplePass(
      "scale every predicate invokation",
      new ScaleAlways(_).rewriteAll,
      removes=Set(features.UnscaledPredicateApplication),
      introduces=Set(/* very simple; only may introduce Scale operator */)),
    "silver-optimize" -> SimplePass("Optimize expressions for Silver", arg => {
      val trs = RewriteSystems.getRewriteSystem("silver_optimize")
      trs.normalize(arg)
    }),
    "chalice-optimize" -> SimplePass("Optimize expressions for Chalice", arg => {
      val trs = RewriteSystems.getRewriteSystem("chalice_optimize")
      trs.normalize(arg)
    }),
    "simplify_expr" -> SimplePass("Simplify expressions", arg => {
      val trs = RewriteSystems.getRewriteSystem("simplify_expr")
      trs.normalize(arg)
    }),
    "simplify_quant" -> SimplePass("Simplify quantifications", arg => {
      val trs = RewriteSystems.getRewriteSystem("simplify_quant_pass1")
      var res = trs.normalize(arg)
      res = RewriteSystems.getRewriteSystem("simplify_quant_pass2").normalize(res)
      res
    }),
    "simplify_sums" -> SimplePass("replace summations with provable functions", arg => {
      val trs = RewriteSystems.getRewriteSystem("summation")
      trs.normalize(arg)
    }),
    "simplify_quant_relations" -> SimplePass("simplify quantified relational expressions", new SimplifyQuantifiedRelations(_).rewriteAll),
    "standardize" -> SimplePass(
      "Standardize representation",
      new Standardize(_).rewriteAll,
      removes=Set(features.NotStandardized)),
    /*"strip_constructors" -> SimplePass(
      "Strip constructors from classes",
      new StripConstructors(_).rewriteAll,
      removes=Set(features.Constructors),
      introduces=Set(/* does what it says on the tin */)),*/
    "create-return-parameter" -> ErrorMapPass(
      "Replace return value by out parameter.",
      new CreateReturnParameter(_, _).rewriteAll,
      permits=Feature.DEFAULT_PERMIT - features.NotFlattened,
      removes=Set(features.NonVoidMethods),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.This,
        features.NotFlattened,
        features.NonVoidMethods,
        features.Arrays,
        features.Inheritance,
        features.ContextEverywhere,
        features.StaticFields,
        features.Constructors,
        features.UnscaledPredicateApplication,
        features.BeforeSilverDomains,
        features.NestedQuantifiers,
        features.InlineQuantifierPattern,
      )),
    "vector-encode" -> SimplePass(
      "Encode vector blocks using the vector library",
      new VectorEncode(_).rewriteAll,
      removes=Set(features.VectorBlock)),
    "chalice-preprocess" -> SimplePass("Pre processing for chalice", new ChalicePreProcess(_).rewriteAll),
    "simple_triggers" -> Pass("Add triggers to quantifiers if possible", (arg, args) => {
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
    "count" -> Pass("Count nodes.", (arg, args) => {
      val cv = new NonLinCountVisitor(arg)
      cv.count()
      if (args.length == 1) Main.counters.put(args(0), cv)
      else Abort("Learn is used without an oracle")
      arg
    }),
    "learn" -> Pass("Learn unit times from counted AST nodes.", (arg, args) => {
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
    "specify-implicit-labels" -> SimplePass(
      "Insert explicit labels for break statements in while loops.",
      new SpecifyImplicitLabels(_).rewriteAll()
    ),
    "break-return-to-goto" -> SimplePass(
      "Rewrite break, return into jumps",
      new BreakReturnToGoto(_).rewriteAll()
    ),
    "break-return-to-exceptions" -> SimplePass(
      "Rewrite break, continue into exceptions",
      new BreakReturnToExceptions(_).rewriteAll()
    ),
    "unfold-switch" -> SimplePass(
      "Unfold switch to chain of if-statements that jump to sections.",
      new UnfoldSwitch(_).rewriteAll()
    ),
    "continue-to-break" -> SimplePass(
      "Convert continues into breaks",
      new ContinueToBreak(_).rewriteAll()
    ),
    "unfold-synchronized" -> SimplePass(
      "Convert synchronized to try-finally",
      new UnfoldSynchronized(_).rewriteAll()
    ),
    "intro-exc-var" -> SimplePass(
      "Introduces the auxiliary sys__exc variable for use by excetional control flow",
      new IntroExcVar(_).rewriteAll()
    ),
    "encode-try-throw-signals" -> SimplePass(
      "Encodes exceptional control flow into gotos and exceptional contracts into regular contracts",
      new EncodeTryThrowSignals(_).rewriteAll()
    ),
    "gen-triggers" -> SimplePass("", Triggers(_).rewriteAll),
    "inline-pattern-to-trigger" -> SimplePass(
      "Explicit inline patterns to normal trigger syntax",
      new InlinePatternToTrigger(_).rewriteAll,
      removes=Set(features.InlineQuantifierPattern),
      introduces=Feature.DEFAULT_INTRODUCE -- Set(
        features.Arrays,
        features.ContextEverywhere,
      ),
    )
  )
}