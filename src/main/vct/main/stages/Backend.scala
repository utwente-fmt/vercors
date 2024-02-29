package vct.main.stages

import hre.io.{RWFile, Writeable}
import hre.progress.Progress
import hre.stages.Stage
import vct.cache.Caches
import vct.col.ast.{Program, Serialize, Verification, VerificationContext}
import vct.col.origin.ExpectedError
import vct.col.rewrite.Generation
import vct.options.{Options, types}
import viper.silver.{ast => silver}
import viper.api.{backend => viper}
import viper.carbon.Carbon
import viper.silicon.Silicon

import java.io.{FileInputStream, FileOutputStream}
import java.nio.file.{Files, Path}
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import scala.runtime.ScalaRunTime

case object Backend {

  def ofOptions(options: Options): Backend = options.backend match {
    case types.Backend.Silicon =>
      val printRawQuantifier = options.siliconPrintQuantifierStats match {
        case Some(freq) => Seq(
          "smt.qi.profile" -> "true",
          "smt.qi.profile_freq" -> s"$freq"
        )
        case None => Seq()
      }
      val z3LogFile = options.devSiliconZ3LogFile match {
        case Some(p) => Seq(
          "trace" -> "true",
          "proof" -> "true",
          "trace-file-name" -> ("\"" + p.toString + "\"")
        )
        case None => Seq()
      }
      val numberOfParallelVerifiers =
        if (options.devSiliconZ3LogFile.isDefined || options.siliconPrintQuantifierStats.isDefined) { Some(1) }
        else { options.devSiliconNumVerifiers }
      SilverBackend(Silicon(
        z3Settings = (printRawQuantifier ++ z3LogFile).toMap,
        z3Path = options.z3Path,
        numberOfParallelVerifiers = numberOfParallelVerifiers,
        timeoutValue = options.devSiliconAssertTimeout,
        totalTimeOut = options.devSiliconTotalTimeout,
        proverLogFile = options.devViperProverLogFile,
        printQuantifierStatistics = options.siliconPrintQuantifierStats.isDefined,
        reportOnNoProgress = options.devSiliconReportOnNoProgress,
        traceBranchConditions = options.devSiliconTraceBranchConditions,
        branchConditionReportInterval = options.devSiliconBranchConditionReportInterval,
        options = options.backendFlags,
      ), options.backendFile, if(options.devCache) Some(Caches.getSiliconDirectory) else None
       , options.skipBackend)

    case types.Backend.Carbon => SilverBackend(Carbon(
      z3Path = options.z3Path,
      boogiePath = options.boogiePath,
      printFile = options.devViperProverLogFile,
      proverLogFile = options.devCarbonBoogieLogFile,
      options = options.backendFlags,
    ), options.backendFile, if(options.devCache) Some(Caches.getSiliconDirectory) else None
     , options.skipBackend)
  }
}

trait Backend extends Stage[Verification[_ <: Generation], Seq[ExpectedError]] {
  override def friendlyName: String = "Verification"
  override def progressWeight: Int = 5

  val skipVerification = false

  def cacheDirectory: Option[Path]

  def cachedDefinitelyVerifiesOrElseUpdate(colProgram: Program[_], update: => Boolean): Unit = {
    val baseDir = cacheDirectory.getOrElse {
      // There is no cache directory: not allowed to skip the update
      update
      return
    }

    val program = Serialize.serialize(colProgram)

    // PB: I am reasonably certain that the hashCode is deterministic in all the elements of the tree: it certainly is
    // for all the case class parts, and ByteString. In any case, the hashCode not being deterministic is fine:
    // it just means the cache will not work very well.

    val path = baseDir.resolve("%02x" format program.hashCode())
    val programFile = path.resolve("program.colpb").toFile

    if(Files.exists(path)) {
      // The result is potentially cached in programFile
      val f = new FileInputStream(programFile)
      val cachedProgram = vct.col.ast.serialize.Program.parseFrom(f)
      f.close()

      if(cachedProgram != program) {
        // Unlikely: in case of a hash collision, just run the verification (permanently unlucky)
        update
      }
    } else if (update) {
      // If the result is not even potentially cached, run update, and if the program definitely verifies, store the result.
      path.toFile.mkdirs()
      val f = new FileOutputStream(programFile)
      program.writeTo(f)
      f.close()
    }
  }

  override def run(in: Verification[_ <: Generation]): Seq[ExpectedError] = {

    val intermediates: Seq[(Program[_ <: Generation], Intermediate)] =
      Progress.map[(VerificationContext[_ <: Generation], Int), (Program[_ <: Generation], Intermediate)](
        in.tasks.zipWithIndex.par, t => s"Transform task ${t._2 + 1}") {  case (task, idx) =>
          (task.program, transform(task.program, idx))}.iterator.to(Seq)

    if(skipVerification) return Seq()

    Progress.foreach[((Program[_ <: Generation], Intermediate), Int)](intermediates.zipWithIndex.par, t => s"Task ${t._2 + 1}") { case (intermediate, _) =>
      cachedDefinitelyVerifiesOrElseUpdate(intermediate._1, verify(intermediate._2))
    }

    in.expectedErrors
  }

  def transform(program: Program[_ <: Generation], idx: Int): Intermediate

  def verify(intermediateProgram: Intermediate): Boolean
}

trait Intermediate
case class SilverIntermediate(intermediate: (silver.Program, Map[Int, vct.col.ast.Node[_]])) extends Intermediate

case class SilverBackend(backend: viper.SilverBackend, output: Option[Path] = None, cacheDirectory: Option[Path] = None, override val skipVerification: Boolean = false) extends Backend {
  override def transform(program: Program[_ <: Generation], idx: Int): Intermediate =
    SilverIntermediate(backend.transform(program, output.map(p => p.resolveSibling(p.getFileName.toString + s"-$idx.vpr"))))

  override def verify(intermediate: Intermediate): Boolean = {
    val SilverIntermediate(intermediateProgram) = intermediate
    backend.submit(intermediateProgram)
  }


}