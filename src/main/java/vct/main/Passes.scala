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
import vct.parsers.JavaResolver
import vct.parsers.rewrite.{FlattenVariableDeclarations, InferADTTypes}

import scala.collection.JavaConverters._
import scala.collection.mutable

object Passes {
  def findPassToRemove(feature: Feature): AbstractPass = BY_KEY.values.find(_.removes.contains(feature)).get

  def computeGoal(program: ProgramUnit, goal: AbstractPass): Seq[AbstractPass] = {
    val visitor = new RainbowVisitor(program)
    program.asScala.foreach(_.accept(visitor))

    var features = visitor.features.toSet
    val toRemove = features -- goal.permits
    var unorderedPasses: mutable.Set[AbstractPass] = mutable.Set() ++ toRemove.map(findPassToRemove)
    var passes: mutable.ArrayBuffer[AbstractPass] = mutable.ArrayBuffer()

    while(unorderedPasses.nonEmpty) {
      val nextPass =
        unorderedPasses.find(pass =>
          (features -- pass.permits).isEmpty &&
          unorderedPasses.forall(_.introduces.intersect(pass.removes).isEmpty)).get

      unorderedPasses -= nextPass
      passes += nextPass
      features = features -- nextPass.removes ++ nextPass.introduces
    }

    passes
  }

  def computeGoalJava(program: ProgramUnit, goal: AbstractPass): util.List[AbstractPass] =
    computeGoal(program, goal).asJava

  def BY_KEY_JAVA: util.Map[String, AbstractPass] = BY_KEY.asJava

  val BY_KEY: Map[String, AbstractPass] = Map(
    "rainbow" -> SimplePass("", arg => {
      computeGoal(arg, BY_KEY("silver"))
      println()
      arg
    }),
    "java" -> SimplePass("print AST in java syntax", arg => {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out, arg)
      out.close()
      arg
    }),
    "c" -> SimplePass("print AST in C syntax", arg => {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      vct.col.ast.print.CPrinter.dump(out, arg)
      out.close()
      arg
    }),
    "add-type-adt" -> SimplePass("Add an ADT that describes the types and use it to implement instanceof", new AddTypeADT(_).rewriteAll),
    "access" -> SimplePass("convert access expressions for histories/futures", new AccessIntroduce(_).rewriteAll),
    "assign" -> SimplePass("change inline assignments to statements", new AssignmentRewriter(_).rewriteAll),
    "silver" -> new AbstractPass("verify input with Silver") {
      override def apply_pass(arg: PassReport, args: Array[String]) = vct.silver.SilverBackend.TestSilicon(arg, args(0))
      override def removes: Set[Feature] = Set()
      override def introduces: Set[Feature] = Set()
      override def permits: Set[Feature] = Set(features.Dereference, features.Null)
    },
    "check" -> SimplePass("run a basic type check", arg => {
      new SimpleTypeCheck(arg).check()
      arg
    }),
    "array_null_values" -> SimplePass("rewrite null values for arrays to None", new ArrayNullValues(_).rewriteAll),
    "pointers_to_arrays" -> SimplePass("rewrite pointers to arrays", new PointersToArrays(_).rewriteAll),
    "desugar_valid_pointer" -> SimplePass("rewrite \\array, \\matrix, \\pointer and \\pointer_index", new DesugarValidPointer(_).rewriteAll),
    "lift_declarations" -> SimplePass("lift declarations to cell of the declared types, to treat locals as heap locations.", new LiftDeclarations(_).rewriteAll),
    "java-check" -> SimplePass("run a Java aware type check", arg => {
      new JavaTypeCheck(arg).check()
      arg
    }),
    "check-defined" -> SimplePass("rewrite process algebra class to check if defined process match their contracts", arg => {
      val tmp = new CheckProcessAlgebra(arg).rewriteAll
      new RandomizedIf(tmp).rewriteAll
    }),
    "check-axioms" -> ErrorMapPass("rewrite process algebra class to check if history axioms are correct", new CheckHistoryAlgebra(_, CheckHistoryAlgebra.Mode.AxiomVerification, _).rewriteAll),
    "check-history" -> ErrorMapPass("rewrite process algebra class to check if history axioms are correct", new CheckHistoryAlgebra(_, CheckHistoryAlgebra.Mode.ProgramVerification, _).rewriteAll),
    "csl-encode" -> new ErrorMapPass("Encode CSL atomic regions with methods", new CSLencoder(_, _).rewriteAll),
    "class-conversion" -> SimplePass("Convert classes into records and procedures", new ClassConversion(_).rewriteAll),
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
    "current_thread" -> SimplePass("Encode references to current thread.", new CurrentThreadRewriter(_).rewriteAll),
    "java-encode" -> SimplePass("Encode Java overloading and inheritance", new JavaEncoder(_).rewriteAll),
    "explicit_encoding" -> SimplePass("encode required and ensured permission as ghost arguments", new ExplicitPermissionEncoding(_).rewriteAll),
    "finalize_args" -> SimplePass("???", new FinalizeArguments(_).rewriteAll),
    "flatten" -> SimplePass("remove nesting of expression", new Flatten(_).rewriteAll),
    "ghost-lift" -> SimplePass("Lift ghost code to real code", new GhostLifter(_).rewriteAll),
    "globalize" -> SimplePass("split classes into static and dynamic parts", new GlobalizeStaticsParameter(_).rewriteAll),
    "ds_inherit" -> SimplePass("rewrite contracts to reflect inheritance, predicate chaining", arg => new DynamicStaticInheritance(arg).rewriteOrdered),
    "flatten_before_after" -> SimplePass("move before/after instructions", new FlattenBeforeAfter(_).rewriteAll),
    "flatten_variable_declarations" -> SimplePass("put the base type in declarations", new FlattenVariableDeclarations(_).rewriteAll),
    "inline" -> SimplePass("Inline all methods marked as inline", new InlinePredicatesRewriter(_).rewriteAll),
    "kernel-split" -> SimplePass("Split kernels into main, thread and barrier.", new KernelRewriter(_).rewriteAll),
    "pvl-encode" -> SimplePass("Encode PVL builtins for verification.", new PVLEncoder(_).rewriteAll),
    "magicwand" -> new ErrorMapPass("Encode magic wand proofs with abstract predicates", new WandEncoder(_, _).rewriteAll),
    "modifies" -> SimplePass("Derive modifies clauses for all contracts", arg => {
      new DeriveModifies().annotate(arg)
      arg
    }),
    "openmp2pvl" -> SimplePass("Compile OpenMP pragmas to PVL", new OpenMPToPVL(_).rewriteAll),
    "parallel_blocks" -> new ErrorMapPass("Encoded the proof obligations for parallel blocks", new ParallelBlockEncoder(_, _).rewriteAll),
    "pvl-compile" -> SimplePass("Compile PVL classes to Java classes", new PVLCompiler(_).rewriteAll),
    "reorder" -> SimplePass("reorder statements (e.g. all declarations at the start of a bock", new ReorderAssignments(_).rewriteAll),
    "standardize-functions" -> SimplePass("translate pure methods to function syntax.", new PureMethodsAsFunctions(_).rewriteAll),
    "java_resolve" -> SimplePass("Resolve the library dependencies of a java program", new JavaResolver(_).rewriteAll),
    "propagate-invariants" -> SimplePass("propagate invariants", new PropagateInvariants(_).rewriteAll),
    "quant-optimize" -> SimplePass("insert satisfyability checks for all methods", new OptimizeQuantifiers(_).rewriteAll),
    "rewrite" -> Pass("Apply a term rewrite system", (arg, args) => {
      val trs = RewriteSystems.getRewriteSystem(args(0))
      trs.normalize(arg)
    }),
    "rewrite_arrays" -> SimplePass("rewrite arrays to sequences of cells", new RewriteArrayRef(_).rewriteAll),
    "generate_adt_functions" -> SimplePass("rewrite  standard operators on sequences to function definitions/calls", new GenerateADTFunctions(_).rewriteAll),
    "infer_adt_types" -> SimplePass("Transform typeless collection constructors by inferring their types.", new InferADTTypes(_).rewriteAll),
    "adt_operator_rewrite" -> SimplePass("rewrite PVL-specific ADT operators", new ADTOperatorRewriter(_).rewriteAll),
    "rm_cons" -> SimplePass("???", new ConstructorRewriter(_).rewriteAll),
    "sat_check" -> SimplePass("insert satisfyability checks for all methods", new SatCheckRewriter(_).rewriteAll),
    "silver-class-reduction" -> SimplePass("reduce classes to single Ref class", new SilverClassReduction(_).rewriteAll),
    "silver-reorder" -> SimplePass("move declarations from inside if-then-else blocks to top", new SilverReorder(_).rewriteAll),
    "scale-always" -> SimplePass("scale every predicate invokation", new ScaleAlways(_).rewriteAll),
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
    "standardize" -> SimplePass("Standardize representation", new Standardize(_).rewriteAll),
    "strip_constructors" -> SimplePass("Strip constructors from classes", new StripConstructors(_).rewriteAll),
    "voidcalls" -> ErrorMapPass("Replace return value by out parameter.", new VoidCalls(_, _).rewriteAll),
    "voidcallsthrown" -> SimplePass("Replace return value and thrown exceptions by out parameters.", new VoidCallsThrown(_).rewriteAll),
    "vector-encode" -> SimplePass("Encode vector blocks using the vector library", new VectorEncode(_).rewriteAll),
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
    })
  )
}