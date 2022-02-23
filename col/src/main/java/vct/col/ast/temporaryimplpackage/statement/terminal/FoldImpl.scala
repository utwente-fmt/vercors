package vct.col.ast.temporaryimplpackage.statement.terminal

import vct.col.ast.Fold
import vct.col.ast.temporaryimplpackage.node.NodeFamilyImpl
import vct.col.ast.temporaryimplpackage.util.CheckFoldUnfoldTarget

trait FoldImpl[G] extends NodeFamilyImpl[G] with CheckFoldUnfoldTarget[G] { this: Fold[G] =>

}