package vct.col.ast.statement.terminal

import vct.col.ast.NormallyCompletingStatement
import vct.col.ast.statement.StatementImpl

trait NormallyCompletingStatementImpl[G] extends StatementImpl[G] { this: NormallyCompletingStatement[G] =>

}
