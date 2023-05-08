package vct.col.util

import vct.col.ast.Node
import vct.result.VerificationError

case class CurrentProgramCheckContext(node: Node[_]) extends VerificationError.Context
