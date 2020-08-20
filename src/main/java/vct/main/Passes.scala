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
  var defined_passes = new util.Hashtable[String, Pass]

  private def branching_pass(defined_passes: util.Hashtable[String, Pass], key: String, description: String, class1: Class[_ <: AbstractRewriter]) =
    try {
      defined_passes.put(key, new Pass(description) {
        private[main] val cons = class1.getConstructor(classOf[ProgramUnit], classOf[ErrorMapping])
        override def apply_pass(inrep: PassReport, args: String*) = {
          val arg = inrep.getOutput
          val res = new PassReport(arg)
          val map = new ErrorMapping(inrep)
          res.add(map)
          try {
            val rw = cons.newInstance(arg, map).asInstanceOf[AbstractRewriter]
            res.setOutput(rw.rewriteAll)
            res
          } catch {
            case e: Exception =>
              throw new HREError("unexpected exception %s", e)
          }
        }
      })
    } catch {
      case e: NoSuchMethodException =>
        Abort("bad rewriter pass %s", key)
    }

  private def compiler_pass(defined_passes: util.Hashtable[String, Pass], key: String, description: String, class1: Class[_ <: AbstractRewriter]) =
    try {
      defined_passes.put(key, new Pass(description) {
        private[main] val cons = class1.getConstructor(classOf[ProgramUnit])
        override def apply(arg: ProgramUnit, args: String*) = {
          try {
            val rw = cons.newInstance(arg).asInstanceOf[AbstractRewriter]
            rw.rewriteAll
          }
          catch {
            case e: Exception =>
              throw new HREError("unexpected exception %s", e)
          }
        }
      })
    } catch {
      case e: NoSuchMethodException =>
        Abort("bad rewriter pass %s", key)
    }

  defined_passes.put("java", new Pass("print AST in java syntax") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      JavaSyntax.getJava(JavaDialect.JavaVerCors).print(out, arg)
      out.close()
      arg
    }
  })
  defined_passes.put("c", new Pass("print AST in C syntax") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      vct.col.ast.print.CPrinter.dump(out, arg)
      out.close()
      arg
    }
  })
  defined_passes.put("add-type-adt", new Pass("Add an ADT that describes the types and use it to implement instanceof") {
    override def apply(arg: ProgramUnit, args: String*) = new AddTypeADT(arg).rewriteAll
  })
  compiler_pass(defined_passes, "access", "convert access expressions for histories/futures", classOf[AccessIntroduce])
  defined_passes.put("assign", new Pass("change inline assignments to statements") {
    override def apply(arg: ProgramUnit, args: String*) = new AssignmentRewriter(arg).rewriteAll
  })
  defined_passes.put("silver", new ValidationPass("verify input with Silver") {
    override def apply_pass(arg: PassReport, args: String*) = vct.silver.SilverBackend.TestSilicon(arg, args(0))
  })
  defined_passes.put("check", new Pass("run a basic type check") {
    override def apply(arg: ProgramUnit, args: String*) = {
      new SimpleTypeCheck(arg).check()
      arg
    }
  })
  defined_passes.put("array_null_values", new Pass("rewrite null values for arrays to None") {
    override def apply(arg: ProgramUnit, args: String*) = new ArrayNullValues(arg).rewriteAll
  })
  defined_passes.put("pointers_to_arrays", new Pass("rewrite pointers to arrays") {
    override def apply(arg: ProgramUnit, args: String*) = new PointersToArrays(arg).rewriteAll
  })
  defined_passes.put("desugar_valid_pointer", new Pass("rewrite \\array, \\matrix, \\pointer and \\pointer_index") {
    override protected def apply(arg: ProgramUnit, args: String*) = new DesugarValidPointer(arg).rewriteAll
  })
  defined_passes.put("lift_declarations", new Pass("lift declarations to cell of the declared types, to treat locals as heap locations.") {
    override def apply(arg: ProgramUnit, args: String*) = new LiftDeclarations(arg).rewriteAll
  })
  defined_passes.put("java-check", new Pass("run a Java aware type check") {
    override def apply(arg: ProgramUnit, args: String*) = {
      new JavaTypeCheck(arg).check()
      arg
    }
  })
  defined_passes.put("check-defined", new Pass("rewrite process algebra class to check if defined process match their contracts") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val tmp = new CheckProcessAlgebra(arg).rewriteAll
      new RandomizedIf(tmp).rewriteAll
    }
  })
  defined_passes.put("check-axioms", new Pass("rewrite process algebra class to check if history axioms are correct") {
    override def apply_pass(arg: PassReport, args: String*) = {
      val input = arg.getOutput
      val res = new PassReport(input)
      val map = new ErrorMapping(arg)
      res.add(map)
      res.setOutput(new CheckHistoryAlgebra(input, CheckHistoryAlgebra.Mode.AxiomVerification, map).rewriteAll)
      res
    }
  })
  defined_passes.put("check-history", new Pass("rewrite process algebra class to check if history accounting is correct") {
    override def apply_pass(arg: PassReport, args: String*) = {
      val input = arg.getOutput
      val res = new PassReport(input)
      val map = new ErrorMapping(arg)
      res.add(map)
      res.setOutput(new CheckHistoryAlgebra(input, CheckHistoryAlgebra.Mode.ProgramVerification, map).rewriteAll)
      res
    }
  })
  defined_passes.put("csl-encode", new Pass("Encode CSL atomic regions with methods") {
    override def apply_pass(arg: PassReport, args: String*) = {
      val input = arg.getOutput
      val res = new PassReport(input)
      val map = new ErrorMapping(arg)
      res.add(map)
      res.setOutput(new CSLencoder(input, map).rewriteAll)
      res
    }
  })
  defined_passes.put("class-conversion", new Pass("Convert classes into records and procedures") {
    override def apply(arg: ProgramUnit, args: String*) = new ClassConversion(arg).rewriteAll
  })
  defined_passes.put("codegen", new Pass("Generate code") {
    override def apply(report: PassReport, arg: ProgramUnit, args: String*): ProgramUnit = {
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
  })
  defined_passes.put("current_thread", new Pass("Encode references to current thread.") {
    override def apply(arg: ProgramUnit, args: String*) = new CurrentThreadRewriter(arg).rewriteAll
  })
  defined_passes.put("java-encode", new Pass("Encode Java overloading and inheritance") {
    override def apply(arg: ProgramUnit, args: String*) = {
      new JavaEncoder(arg).rewriteAll
    }
  })
  defined_passes.put("explicit_encoding", new Pass("encode required and ensured permission as ghost arguments") {
    override def apply(arg: ProgramUnit, args: String*) = new ExplicitPermissionEncoding(arg).rewriteAll
  })
  defined_passes.put("finalize_args", new Pass("???") {
    override def apply(arg: ProgramUnit, args: String*) = new FinalizeArguments(arg).rewriteAll
  })
  defined_passes.put("flatten", new Pass("remove nesting of expression") {
    override def apply(arg: ProgramUnit, args: String*) = new Flatten(arg).rewriteAll
  })
  defined_passes.put("ghost-lift", new Pass("Lift ghost code to real code") {
    override def apply(arg: ProgramUnit, args: String*) = new GhostLifter(arg).rewriteAll
  })
  defined_passes.put("globalize", new Pass("split classes into static and dynamic parts") {
    override def apply(arg: ProgramUnit, args: String*) = new GlobalizeStaticsParameter(arg).rewriteAll
  })
  defined_passes.put("ds_inherit", new Pass("rewrite contracts to reflect inheritance, predicate chaining") {
    override def apply(arg: ProgramUnit, args: String*) = new DynamicStaticInheritance(arg).rewriteOrdered
  })
  defined_passes.put("flatten_before_after", new Pass("move before/after instructions") {
    override def apply(arg: ProgramUnit, args: String*) = new FlattenBeforeAfter(arg).rewriteAll
  })
  defined_passes.put("flatten_variable_declarations", new Pass("put the base type in declarations") {
    override def apply(arg: ProgramUnit, args: String*) = new FlattenVariableDeclarations(arg).rewriteAll
  })
  defined_passes.put("inline", new Pass("Inline all methods marked as inline") {
    override def apply(arg: ProgramUnit, args: String*) = new InlinePredicatesRewriter(arg).rewriteAll
  })
  defined_passes.put("kernel-split", new Pass("Split kernels into main, thread and barrier.") {
    override def apply(arg: ProgramUnit, args: String*) = new KernelRewriter(arg).rewriteAll
  })
  defined_passes.put("pvl-encode", new Pass("Encode PVL builtins for verification.") {
    override def apply(arg: ProgramUnit, args: String*) = new PVLEncoder(arg).rewriteAll
  })
  branching_pass(defined_passes, "magicwand", "Encode magic wand proofs with abstract predicates", classOf[WandEncoder])
  defined_passes.put("modifies", new Pass("Derive modifies clauses for all contracts") {
    override def apply(arg: ProgramUnit, args: String*) = {
      new DeriveModifies().annotate(arg)
      arg
    }
  })
  defined_passes.put("openmp2pvl", new Pass("Compile OpenMP pragmas to PVL") {
    override def apply(arg: ProgramUnit, args: String*) = new OpenMPToPVL(arg).rewriteAll
  })
  defined_passes.put("parallel_blocks", new Pass("Encoded the proof obligations for parallel blocks") {
    override def apply_pass(arg: PassReport, args: String*) = {
      val input = arg.getOutput
      val res = new PassReport(input)
      val map = new ErrorMapping(arg)
      res.add(map)
      res.setOutput(new ParallelBlockEncoder(input, map).rewriteAll)
      res
    }
  })
  defined_passes.put("pvl-compile", new Pass("Compile PVL classes to Java classes") {
    override def apply(arg: ProgramUnit, args: String*) = new PVLCompiler(arg).rewriteAll
  })
  defined_passes.put("reorder", new Pass("reorder statements (e.g. all declarations at the start of a bock") {
    override def apply(arg: ProgramUnit, args: String*) = new ReorderAssignments(arg).rewriteAll
  })
  defined_passes.put("standardize-functions", new Pass("translate pure methods to function syntax.") {
    override def apply(arg: ProgramUnit, args: String*) = new PureMethodsAsFunctions(arg).rewriteAll
  })
  defined_passes.put("java_resolve", new Pass("Resolve the library dependencies of a java program") {
    override def apply(arg: ProgramUnit, args: String*) = new JavaResolver(arg).rewriteAll
  })
  defined_passes.put("propagate-invariants", new Pass("propagate invariants") {
    override def apply(arg: ProgramUnit, args: String*) = new PropagateInvariants(arg).rewriteAll
  })
  defined_passes.put("quant-optimize", new Pass("insert satisfyability checks for all methods") {
    override def apply(arg: ProgramUnit, args: String*) = new OptimizeQuantifiers(arg).rewriteAll
  })
  defined_passes.put("rewrite", new Pass("Apply a term rewrite system") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val trs = RewriteSystems.getRewriteSystem(args(0))
      trs.normalize(arg)
    }
  })
  defined_passes.put("rewrite_arrays", new Pass("rewrite arrays to sequences of cells") {
    override def apply(arg: ProgramUnit, args: String*) = new RewriteArrayRef(arg).rewriteAll
  })
  defined_passes.put("generate_adt_functions", new Pass("rewrite  standard operators on sequences to function definitions/calls") {
    override def apply(arg: ProgramUnit, args: String*) = new GenerateADTFunctions(arg).rewriteAll
  })
  defined_passes.put("infer_adt_types", new Pass("Transform typeless collection constructors by inferring their types.") {
    override def apply(arg: ProgramUnit, args: String*) = new InferADTTypes(arg).rewriteAll
  })
  defined_passes.put("adt_operator_rewrite", new Pass("rewrite PVL-specific ADT operators") {
    override def apply(arg: ProgramUnit, args: String*) = new ADTOperatorRewriter(arg).rewriteAll
  })
  defined_passes.put("rm_cons", new Pass("???") {
    override def apply(arg: ProgramUnit, args: String*) = new ConstructorRewriter(arg).rewriteAll
  })
  defined_passes.put("sat_check", new Pass("insert satisfyability checks for all methods") {
    override def apply(arg: ProgramUnit, args: String*) = new SatCheckRewriter(arg).rewriteAll
  })
  defined_passes.put("silver-class-reduction", new Pass("reduce classes to single Ref class") {
    override def apply(arg: ProgramUnit, args: String*) = new SilverClassReduction(arg).rewriteAll
  })
  defined_passes.put("silver-reorder", new Pass("move declarations from inside if-then-else blocks to top") {
    override def apply(arg: ProgramUnit, args: String*) = new SilverReorder(arg).rewriteAll
  })
  defined_passes.put("scale-always", new Pass("scale every predicate invokation") {
    override def apply(arg: ProgramUnit, args: String*) = new ScaleAlways(arg).rewriteAll
  })
  defined_passes.put("silver-optimize", new Pass("Optimize expressions for Silver") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val trs = RewriteSystems.getRewriteSystem("silver_optimize")
      trs.normalize(arg)
    }
  })
  defined_passes.put("chalice-optimize", new Pass("Optimize expressions for Chalice") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val trs = RewriteSystems.getRewriteSystem("chalice_optimize")
      trs.normalize(arg)
    }
  })
  defined_passes.put("simplify_expr", new Pass("Simplify expressions") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val trs = RewriteSystems.getRewriteSystem("simplify_expr")
      trs.normalize(arg)
    }
  })
  defined_passes.put("simplify_quant", new Pass("Simplify quantifications") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val trs = RewriteSystems.getRewriteSystem("simplify_quant_pass1")
      var res = trs.normalize(arg)
      res = RewriteSystems.getRewriteSystem("simplify_quant_pass2").normalize(res)
      res
    }
  })
  defined_passes.put("simplify_sums", new Pass("replace summations with provable functions") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val trs = RewriteSystems.getRewriteSystem("summation")
      trs.normalize(arg)
    }
  })
  defined_passes.put("simplify_quant_relations", new Pass("simplify quantified relational expressions") {
    override def apply(arg: ProgramUnit, args: String*) = new SimplifyQuantifiedRelations(arg).rewriteAll
  })
  defined_passes.put("standardize", new Pass("Standardize representation") {
    override def apply(arg: ProgramUnit, args: String*) = new Standardize(arg).rewriteAll
  })
  defined_passes.put("strip_constructors", new Pass("Strip constructors from classes") {
    override def apply(arg: ProgramUnit, args: String*) = new StripConstructors(arg).rewriteAll
  })
  branching_pass(defined_passes, "voidcalls", "Replace return value by out parameter.", classOf[VoidCalls])
  defined_passes.put("voidcallsthrown", new Pass("Replace return value and thrown exceptions by out parameters.") {
    override def apply(arg: ProgramUnit, args: String*) = new VoidCallsThrown(arg).rewriteAll
  })
  compiler_pass(defined_passes, "vector-encode", "Encode vector blocks using the vector library", classOf[VectorEncode])
  defined_passes.put("chalice-preprocess", new Pass("Pre processing for chalice") {
    override def apply(arg: ProgramUnit, args: String*) = new ChalicePreProcess(arg).rewriteAll
  })
  defined_passes.put("simple_triggers", new Pass("Add triggers to quantifiers if possible") {
    override def apply(arg: ProgramUnit, args: String*) = {
      var res = arg
      val `val` = Integer.valueOf(args(0))
      // First gather quantified variables for quantifiers without triggers.
      res = new OptimizeQuantifiers(res).rewriteAll
      // For quantifiers without triggers, and complex subscripts not containing quantified variables, add quantifier variable equal to the complex subscript.
      if ((`val` & 2) > 0) res = new RewriteComplexUnitSubscripts(res).rewriteAll
      // Try to add triggers for the now possibly simplified quantifiers.
      if ((`val` & 1) > 0) res = new AddSimpleTriggers(res).rewriteAll
      res
    }
  })
  defined_passes.put("count", new Pass("Count nodes.") {
    override def apply(arg: ProgramUnit, args: String*) = {
      val cv = new NonLinCountVisitor(arg)
      cv.count()
      if (args.length == 1) Main.counters.put(args(0), cv)
      else Abort("Learn is used without an oracle")
      arg
    }
  })
  defined_passes.put("learn", new Pass("Learn unit times from counted AST nodes.") {
    override def apply(arg: ProgramUnit, args: String*) = {
      if (args.length == 1) {
        val start_time = args(0).toLong
        val time = System.currentTimeMillis - start_time
        for (entry <- Main.counters.entrySet.asScala) {
          Oracle.tell(entry.getKey, entry.getValue, time)
        }
      }
      else Abort("Learn is used without a starting time.")
      arg
    }
  })
}