package vct.col.ast.statement.terminal

import vct.col.ast.Assign
import vct.col.ast.ops.AssignOps
import vct.col.print._

trait AssignImpl[G] extends AssignOps[G] { this: Assign[G] =>
}
