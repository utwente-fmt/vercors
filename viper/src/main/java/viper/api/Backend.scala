package viper.api

import vct.col.ast.Program

trait Backend {
  def submit(program: Program): Unit
}
