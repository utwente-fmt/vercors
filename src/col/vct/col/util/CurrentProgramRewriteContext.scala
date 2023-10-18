package vct.col.util

import vct.col.ast.Program
import vct.result.VerificationError

case class CurrentProgramRewriteContext(program: Program[_])
    extends VerificationError.Context
