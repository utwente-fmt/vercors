package vct.main

import hre.config.BooleanSetting
import hre.config.StringSetting
import hre.lang.HREError
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.ASTClass
import vct.col.ast.stmt.decl.ASTSpecial
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.syntax.JavaDialect
import vct.col.ast.syntax.JavaSyntax
import vct.col.ast.syntax.Syntax
import vct.col.ast.util.AbstractRewriter
import vct.col.rewrite._
import vct.col.util.JavaTypeCheck
import vct.col.util.SimpleTypeCheck
import vct.experiments.learn.NonLinCountVisitor
import vct.experiments.learn.Oracle
import vct.experiments.learn.SpecialCountVisitor
import vct.logging.ErrorMapping
import vct.logging.ExceptionMessage
import vct.logging.PassReport
import vct.parsers.JavaResolver
import vct.parsers.rewrite.FlattenVariableDeclarations
import vct.parsers.rewrite.InferADTTypes
import java.io.File
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.PrintWriter
import java.lang.reflect.Constructor
import java.util

import hre.lang.System.Abort

import scala.collection.JavaConverters._

object Passes {
  def BY_KEY_JAVA: util.Map[String, AbstractPass] = BY_KEY.asJava

  val BY_KEY = Map(
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
    },
    "check" -> SimplePass("run a basic type check", arg => {
      new SimpleTypeCheck(arg).check()
      arg
    }),
    "array_null_values" -> SimplePass("rewrite null values for arrays to None", new ArrayNullValues(_).rewriteAll),
    "pointers_to_arrays" -> SimplePass("rewrite pointers to arrays", new PointersToArrays(_).rewriteAll),
    "desugar_valid_pointer" -> new AbstractPass("rewrite \\array, \\matrix, \\pointer and \\pointer_index") {
      override protected def apply(arg: ProgramUnit, args: Array[String]) = new DesugarValidPointer(arg).rewriteAll
    },
    "lift_declarations" -> SimplePass("lift declarations to cell of the declared types, to treat locals as heap locations.", new LiftDeclarations(_).rewriteAll),
    "java-check" -> SimplePass("run a Java aware type check", arg => {
      new JavaTypeCheck(arg).check()
      arg
    }),
    "check-defined" -> SimplePass("rewrite process algebra class to check if defined process match their contracts", arg => {
      val tmp = new CheckProcessAlgebra(arg).rewriteAll
      new RandomizedIf(tmp).rewriteAll
    }),
    "check-axioms" -> new AbstractPass("rewrite process algebra class to check if history axioms are correct") {
      override def apply_pass(arg: PassReport, args: Array[String]) = {
        val input = arg.getOutput
        val res = new PassReport(input)
        val map = new ErrorMapping(arg)
        res.add(map)
        res.setOutput(new CheckHistoryAlgebra(input, CheckHistoryAlgebra.Mode.AxiomVerification, map).rewriteAll)
        res
      }
    },
    "check-history" -> new AbstractPass("rewrite process algebra class to check if history accounting is correct") {
      override def apply_pass(arg: PassReport, args: Array[String]) = {
        val input = arg.getOutput
        val res = new PassReport(input)
        val map = new ErrorMapping(arg)
        res.add(map)
        res.setOutput(new CheckHistoryAlgebra(input, CheckHistoryAlgebra.Mode.ProgramVerification, map).rewriteAll)
        res
      }
    },
    "csl-encode" -> new AbstractPass("Encode CSL atomic regions with methods") {
      override def apply_pass(arg: PassReport, args: Array[String]) = {
        val input = arg.getOutput
        val res = new PassReport(input)
        val map = new ErrorMapping(arg)
        res.add(map)
        res.setOutput(new CSLencoder(input, map).rewriteAll)
        res
      }
    },
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
    "magicwand" -> new AbstractPass("Encode magic wand proofs with abstract predicates") {
      override def apply_pass(reportIn: PassReport, args: Array[String]): PassReport = {
        val arg = reportIn.getOutput
        val reportOut = new PassReport(arg)
        val map = new ErrorMapping(reportIn)
        try {
          val result = new WandEncoder(arg, map).rewriteAll
          reportOut.setOutput(result)
          reportOut
        } catch {
          case e: Exception =>
            throw new HREError("unexpected exception %s", e)
        }
      }
    },
    "modifies" -> SimplePass("Derive modifies clauses for all contracts", arg => {
      new DeriveModifies().annotate(arg)
      arg
    }),
    "openmp2pvl" -> SimplePass("Compile OpenMP pragmas to PVL", new OpenMPToPVL(_).rewriteAll),
    "parallel_blocks" -> new AbstractPass("Encoded the proof obligations for parallel blocks") {
      override def apply_pass(arg: PassReport, args: Array[String]) = {
        val input = arg.getOutput
        val res = new PassReport(input)
        val map = new ErrorMapping(arg)
        res.add(map)
        res.setOutput(new ParallelBlockEncoder(input, map).rewriteAll)
        res
      }
    },
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
    "voidcalls" -> new AbstractPass("Replace return value by out parameter.") {
      override def apply_pass(reportIn: PassReport, args: Array[String]): PassReport = {
        val arg = reportIn.getOutput
        val reportOut = new PassReport(arg)
        val map = new ErrorMapping(reportIn)
        try {
          val result = new VoidCalls(arg, map).rewriteAll
          reportOut.setOutput(result)
          reportOut
        } catch {
          case e: Exception =>
            throw new HREError("unexpected exception %s", e)
        }
      }
    },
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