package vct.options

import vct.options.types.{Backend, PathOrStd}
import vct.resources.Resources

import java.nio.file.Path

case class TransformationOptions(
    backend: Backend = Backend.Silicon,
    backendFile: Option[Path] = None,
    outputAfterPass: Map[String, PathOrStd] = Map.empty,
    outputBeforePass: Map[String, PathOrStd] = Map.empty,
    outputIntermediatePrograms: Option[Path] = None,
    backendFlags: Seq[String] = Nil,
    skipBackend: Boolean = false,
    simplifyPaths: Seq[PathOrStd] = Seq("pushin", "simplify")
      .map(name => PathOrStd.Path(Resources.getSimplificationPath(name))),
    simplifyPathsAfterRelations: Seq[PathOrStd] = Seq("simplify")
      .map(name => PathOrStd.Path(Resources.getSimplificationPath(name))),
    adtPath: Path = Resources.getAdtPath,
    devAbruptExc: Boolean = false,
    devCheckSat: Boolean = true,
    devSimplifyDebugIn: Seq[String] = Nil,
    devSimplifyDebugMatch: Boolean = false,
    devSimplifyDebugMatchShort: Boolean = true,
    devSimplifyDebugNoMatch: Boolean = false,
    devSimplifyDebugFilterInputKind: Option[String] = None,
    devSimplifyDebugFilterRule: Option[String] = None,
    devSplitVerificationByProcedure: Boolean = false,
)
