package viper.api.backend

import vct.col.ast.Program

import java.nio.file.Path

trait Backend {
  def submit(program: Program[_], output: Option[Path]): Boolean
}
