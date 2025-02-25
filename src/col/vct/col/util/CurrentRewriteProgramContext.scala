package vct.col.util

import vct.col.ast.Program
import vct.col.print.Doc
import vct.result.VerificationError

case class CurrentRewriteProgramContext(program: Program[_])
    extends CurrentProgramContext
