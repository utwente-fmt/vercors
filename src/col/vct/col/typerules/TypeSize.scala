package vct.col.typerules

case object TypeSize {
  case class Unknown() extends TypeSize
  case class Exact(size: BigInt) extends TypeSize
  case class Minimally(size: BigInt) extends TypeSize

  def struct(sizes: TypeSize*): TypeSize =
    sizes.reduce[TypeSize] {
      case (Unknown(), _) | (_, Unknown()) => Unknown()
      case (Minimally(a), Minimally(b)) => Minimally(a + b)
      case (Minimally(a), Exact(b)) => Minimally(a + b)
      case (Exact(a), Minimally(b)) => Minimally(a + b)
      case (Exact(a), Exact(b)) => Minimally(a + b)
    }

  def packed(sizes: TypeSize*): TypeSize =
    sizes.reduce[TypeSize] {
      case (Unknown(), _) | (_, Unknown()) => Unknown()
      case (Minimally(a), Minimally(b)) => Minimally(a + b)
      case (Minimally(a), Exact(b)) => Minimally(a + b)
      case (Exact(a), Minimally(b)) => Minimally(a + b)
      case (Exact(a), Exact(b)) => Exact(a + b)
    }
}

sealed trait TypeSize extends Ordered[TypeSize] {
  import TypeSize._

  def getExact: BigInt =
    this match {
      case TypeSize.Unknown() | TypeSize.Minimally(_) =>
        throw new IllegalArgumentException("Expected an exact size")
      case TypeSize.Exact(size) => size
    }

  override def compare(other: TypeSize): Int =
    (this, other) match {
      // Unknown is bigger than all
      case (Unknown(), Unknown()) => 0
      case (Unknown(), _) => 1
      case (_, Unknown()) => -1
      // Minimally is bigger than exact
      case (Minimally(a), Minimally(b)) => a.compare(b)
      case (Exact(_), Minimally(_)) => -1
      case (Minimally(_), Exact(_)) => 1
      case (Exact(a), Exact(b)) => a.compare(b)
    }
}
