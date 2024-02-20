package vct.rewrite.rasi

case object Utils {
  def abs_max(a: Int, b: Int): Int = Seq(-a, a, -b, b).max

  def prod_max(a1: Int, a2: Int, b1: Int, b2: Int): Int = Seq(a1 * b1, a1 * b2, a2 * b1, a2 * b2).max

  def prod_min(a1: Int, a2: Int, b1: Int, b2: Int): Int = Seq(a1 * b1, a1 * b2, a2 * b1, a2 * b2).min
}
