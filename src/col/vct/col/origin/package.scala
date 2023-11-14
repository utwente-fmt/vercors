package vct.col

package object origin {
  val DiagnosticOrigin: Origin = Origin(Seq(LabelContext("diagnostic")))
  val FileSpanningOrigin: Origin = Origin(Seq(LabelContext("multiple files")))
}
