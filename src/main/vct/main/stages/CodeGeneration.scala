package vct.main.stages

import hre.progress.Progress
import hre.stages.Stage
import vct.col.ast.{JavaClass, Node, Verification}
import vct.col.print.Ctx
import vct.col.rewrite.{Generation, PrettifyBlocks, RewriterBuilder}
import vct.importer.Util
import vct.main.stages.Transformation.writeOutFunctions
import vct.options.Options
import vct.options.types.{Backend, PathOrStd}
import vct.rewrite.runtime.GenerateJava
import vct.rewrite.veymont.ParalleliseEndpoints

object CodeGeneration {

  private def writeOutFunctions(m: Map[String, PathOrStd]): Seq[(String, Verification[_ <: Generation] => Unit)] =
    m.toSeq.map {
      case (key, out) => (key, (program: Verification[_ <: Generation]) => out.write { writer =>
        program.write(writer)(Ctx().namesIn(program))
      })
    }

  def veymontGenerationOfOptions(options: Options): VeyMontGeneration =
    options.backend match {
      case Backend.Silicon | Backend.Carbon =>
        VeyMontGeneration(
          onBeforePassKey = writeOutFunctions(options.outputBeforePass),
          onAfterPassKey = writeOutFunctions(options.outputAfterPass),
          channelClass = Util.loadJavaClass(options.veymontChannel),
        )
    }

  def runtimeGenerationOfOptions(options: Options): RuntimeGeneration =
    RuntimeGeneration(
      onBeforePassKey = writeOutFunctions(options.outputBeforePass),
      onAfterPassKey = writeOutFunctions(options.outputAfterPass)
    )
}

class CodeGeneration(val onBeforePassKey: Seq[(String, Verification[_ <: Generation] => Unit)],
                     val onAfterPassKey: Seq[(String, Verification[_ <: Generation] => Unit)],
                     val passes: Seq[RewriterBuilder]
                       ) extends Stage[Verification[_ <: Generation], Verification[_ <: Generation]] {
  override def friendlyName: String = "Generating VeyMont output"

  override def progressWeight: Int = 1

  override def run(input: Verification[_ <: Generation]): Verification[_ <: Generation] = {
    var result: Verification[_ <: Generation] = input
    Progress.foreach(passes, (pass: RewriterBuilder) => pass.key) { pass =>
      onBeforePassKey.foreach {
        case (key, action) => if (pass.key == key) action(result)
      }

      result = pass().dispatch(result)

      onAfterPassKey.foreach {
        case (key, action) => if (pass.key == key) action(result)
      }

      result = PrettifyBlocks().dispatch(result)
    }
    result
  }

}

case class VeyMontGeneration(override val onBeforePassKey: Seq[(String, Verification[_ <: Generation] => Unit)] = Nil,
                             override val onAfterPassKey: Seq[(String, Verification[_ <: Generation] => Unit)] = Nil,
                             channelClass: JavaClass[_])
  extends CodeGeneration(onBeforePassKey, onAfterPassKey, Seq(
    ParalleliseEndpoints.withArg(channelClass),
  ))

case class RuntimeGeneration(override val onBeforePassKey: Seq[(String, Verification[_ <: Generation] => Unit)] = Nil,
                             override val onAfterPassKey: Seq[(String, Verification[_ <: Generation] => Unit)] = Nil)
  extends CodeGeneration(onBeforePassKey, onAfterPassKey, Seq(
    GenerateJava
  ))
