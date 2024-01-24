import mill.{Agg, define}
import mill.scalalib.DepSyntax

package object settings {
  val root = os.pwd
  val src = root / "src"
  val test = root / "test"
  val res = root / "res"
  val lib = root / "lib"
  val docs = root / "docs"
  val meta = root / "out" / "mill-build"

  object deps {
    val log = Agg(
      ivy"com.typesafe.scala-logging::scala-logging:3.9.5",
      ivy"ch.qos.logback:logback-classic:1.4.5",
    )

    val common = log ++ Agg(
      ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4",
      ivy"io.spray::spray-json:1.3.6"
    )
  }
}
