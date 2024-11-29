package vct.parsers.transform.systemctocol.util

import vct.col.ast.{FilterMode, Include, Exclude, NeutralFilterMode}

object AstHelpers {
  def neutral[G](): FilterMode[G] = NeutralFilterMode()
  def include[G](): FilterMode[G] = Include()
  def exclude[G](): FilterMode[G] = Exclude()
}
