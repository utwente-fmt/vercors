package vct.col

package object origin {
  object TraceOrigin {
    def apply()(implicit line: sourcecode.Line, file: sourcecode.File): Origin =
      Origin(Seq(LabelContext(s"${file.value}:${line.value}")))
  }
  val DiagnosticOrigin: Origin = Origin(Seq(LabelContext("diagnostic")))
  val FileSpanningOrigin: Origin = Origin(Seq(LabelContext("multiple files")))
}
