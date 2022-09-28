package viper.api.backend

import hre.io.Writeable
import vct.col.ast.Program

trait Backend {
  def submit(program: Program[_], output: Option[Writeable]): Unit
}
