package util

import mill.{Agg, PathRef, T, pathReadWrite}
import mill.scalalib.Assembly
import os.Path

trait SeparatePackedResourcesModule extends JavaModule {
  def bareResourcePaths: T[Seq[Path]] = T { Seq.empty[Path] }
  def bareResources = T.sources { bareResourcePaths().map(PathRef(_, quick = true)) }

  def packedResources: T[Seq[PathRef]]

  override final def resources = T.sources {
    bareResources() ++ packedResources()
  }

  private def nilTask: T[Seq[Path]] = T { Seq.empty[Path] }

  def transitiveBareResourcePaths = T {
    T.traverse(
      (moduleDeps ++ compileModuleDeps).flatMap(_.transitiveModuleDeps).distinct
    ) {
      case module: SeparatePackedResourcesModule => module.bareResourcePaths
      case other => nilTask
    }().flatten
  }

  def bareClasspath = T {
    bareResourcePaths() ++ transitiveBareResourcePaths()
  }

  def localPackedClasspath = T {
    (Nil/*localCompileClasspath().toSeq*/) ++ packedResources() ++ Agg(compile().classes)
  }

  def transitiveLocalPackedClasspath = T {
    (T.traverse(
      (moduleDeps ++ compileModuleDeps).flatMap(_.transitiveModuleDeps).distinct
    ) {
      case module: SeparatePackedResourcesModule => module.localPackedClasspath
      case m => m.localClasspath
    })().flatten
  }

  override def upstreamAssembly = T {
    Assembly.createAssembly(
      (transitiveLocalPackedClasspath() ++
        unmanagedClasspath() ++
        resolvedRunIvyDeps()
        ).map(_.path),
      manifest(),
      assemblyRules = assemblyRules
    )
  }

  override def assembly = T {
    Assembly.createAssembly(
      Agg.from(localPackedClasspath().map(_.path)),
      manifest(),
      prependShellScript(),
      Some(upstreamAssembly().path),
      assemblyRules
    )
  }
}