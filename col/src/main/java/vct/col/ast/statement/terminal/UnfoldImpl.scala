package vct.col.ast.statement.terminal

import vct.col.ast.Unfold
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.util.CheckFoldUnfoldTarget

trait UnfoldImpl[G] extends NodeFamilyImpl[G] with CheckFoldUnfoldTarget[G] { this: Unfold[G] =>

}