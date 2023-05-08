package vct.col.util

import vct.col.ast.Declaration
import vct.result.VerificationError

case class ConstructingSuccessorOfContext(decl: Declaration[_]) extends VerificationError.Context
