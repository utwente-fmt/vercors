package vct.col.typerules

case object PlatformContext {
  // The minimum (16 bits) specified for the pointer is not specified by the standard
  val DEFAULT: PlatformContext = PlatformContext(
    charSize = TypeSize.Minimally(8),
    shortSize = TypeSize.Minimally(16),
    intSize = TypeSize.Minimally(16),
    longSize = TypeSize.Minimally(32),
    longLongSize = TypeSize.Minimally(64),
    pointerSize = TypeSize.Minimally(16),
  )
}

case class PlatformContext(
    charSize: TypeSize,
    shortSize: TypeSize,
    intSize: TypeSize,
    longSize: TypeSize,
    longLongSize: TypeSize,
    pointerSize: TypeSize,
)
