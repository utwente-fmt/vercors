package vct.col.ast.node

import vct.col.ast.VerificationContext

/**
 * Verifies the program in isolation, which may be useful when proof obligations are also axiomatized. tryAssumeFunctions
 * and tryAssumePredicates respectively instruct the proof backend to try not to verify the functions and predicates,
 * though errors reported on them must be handled correctly (e.g. deduplicated) anyway, in case the backend does not
 * support this.
 */
trait VerificationContextImpl[G] { this: VerificationContext[G] =>

}
