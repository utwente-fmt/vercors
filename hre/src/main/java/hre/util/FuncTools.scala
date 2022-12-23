package hre.util

case object FuncTools {
  def repeat[T](f: T => T, n: Int, arg: T): T =
    (0 until n).foldLeft(arg)((res, _) => f(res))

  def firstOption[T](options: Seq[Option[T]]): Option[T] =
    options.foldLeft(Option.empty[T])(_ orElse _)

  def firstOption[In, Out](data: Seq[In], func: Function[In, Option[Out]]): Option[Out] =
    data.foldLeft(Option.empty[Out])(_ orElse func(_))
}
