package vct.rewrite.rasi

case object Utils {
  def absmax(a: Int, b: Int): Int = Seq(-a, a, -b, b).max
}
