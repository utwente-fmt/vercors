package viper.silver.utility

trait TimingUtils {
  /** Formats a time in milliseconds. */
  def formatTime(millis: Long): String = {
    if (millis > 1000) "%.2f sec".format(millis * 1.0 / 1000)
    else "%s msec".format(millis.toString)
  }

  /**
    * Measures the time it takes to execute `f` and returns the result of `f`
    * as well as the required time.
    */
  def time[T](f: () => T): (T, Long) = {
    val start = System.currentTimeMillis()
    val r = f.apply()

    (r, System.currentTimeMillis() - start)
  }
}
