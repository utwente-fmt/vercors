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

sealed trait TypeSize {}
