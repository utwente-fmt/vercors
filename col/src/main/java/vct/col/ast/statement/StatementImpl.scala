package vct.col.ast.statement

import vct.col.ast.Statement
import vct.col.ast.node.NodeFamilyImpl

trait StatementImpl[G] extends NodeFamilyImpl[G] { this: Statement[G] =>

}
