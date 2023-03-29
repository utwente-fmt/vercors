package vct.col.ast.statement.terminal

import vct.col.ast.Fold
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.util.CheckFoldUnfoldTarget

trait FoldImpl[G] extends NodeFamilyImpl[G] with CheckFoldUnfoldTarget[G] { this: Fold[G] =>

}