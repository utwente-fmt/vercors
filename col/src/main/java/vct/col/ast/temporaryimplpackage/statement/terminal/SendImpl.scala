package vct.col.ast.temporaryimplpackage.statement.terminal

import vct.col.ast.Send

// PB: send and recv should receive some syntax updates: I think a built-in condition is useful (now collected from if
// statements) and the offset should be a constant, rather than an expression (since they need to be one-to-one).
// Perhaps we also shouldn't lean on labels, and instead name the statements themselves or so.
trait SendImpl { this: Send =>

}