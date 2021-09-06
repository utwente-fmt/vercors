package hre.util

case object FuncTools {
  def repeat[T](f: T => T, n: Int, arg: T): T =
    (0 until n).foldLeft(arg)((res, _) => f(res))
}
