package vct.col.ast.temporaryimplpackage.statement.terminal

import vct.col.ast.Unfold
import vct.col.ast.temporaryimplpackage.node.NodeFamilyImpl
import vct.col.ast.temporaryimplpackage.util.CheckFoldUnfoldTarget

trait UnfoldImpl[G] extends NodeFamilyImpl[G] with CheckFoldUnfoldTarget[G] { this: Unfold[G] =>

}