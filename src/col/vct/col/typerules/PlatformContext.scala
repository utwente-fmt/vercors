package vct.col.typerules

case object PlatformContext {
  // The minimum (16 bits) specified for the pointer is not specified by the standard
  // The size of char (8 bits) in bits is specified as a minimum in the C standard but since we are hard-coding
  // 8-bit bytes and sizeof(char) is guaranteed to be 1 this requires char to be 8 bits
  val DEFAULT: PlatformContext = PlatformContext(
    charSize = TypeSize.Exact(8),
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
