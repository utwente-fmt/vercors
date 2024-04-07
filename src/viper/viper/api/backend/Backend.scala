package viper.api.backend

import vct.col.ast.Program

import java.nio.file.Path

trait Backend[P] {
  def transform(program: Program[_], output: Option[Path]): P
  def submit(intermediateProgram: P): Boolean
}
