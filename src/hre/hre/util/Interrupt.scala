package hre.util

object Interrupt {
  def check(): Unit =
    if (Thread.interrupted()) { throw new InterruptedException() }
}
