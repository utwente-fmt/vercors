package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import vct.col.ast._
import vct.col.newrewrite.ApplyTermRewriter
import vct.col.origin.Origin
import vct.col.rewrite.{InitialGeneration, Rewritten}
import vct.col.util.AstBuildHelpers._

import java.nio.file.Paths

case class ApplyTermRewriterSpec() extends AnyFlatSpec with should.Matchers {
  case class Named(name: String) extends Origin {
    override def preferredName: String = name
    override def context: String = ""
  }

  it should "do some stuff" in {
    val rw = ApplyTermRewriter.BuilderForFile(Paths.get("src/main/universal/res/config/pushin.pvl"))[InitialGeneration]()
    val rw2 = ApplyTermRewriter.BuilderForFile(Paths.get("src/main/universal/res/config/simplify.pvl"))[Rewritten[InitialGeneration]]()

    implicit val o: Origin = Named("unknown")

    val cond = new Variable[InitialGeneration](TBool())(Named("cond"))
    val i = new Variable[InitialGeneration](TInt())(Named("i"))
    val qcond = new Variable[InitialGeneration](TBool())(Named("qcond"))
    val r1 = new Variable[InitialGeneration](TResource())(Named("r1"))
    val r2 = new Variable[InitialGeneration](TResource())(Named("r2"))
    val r3 = new Variable[InitialGeneration](TBool())(Named("r3"))

    val in = Scope(Seq(cond, qcond, r1, r2, r3), Eval(Implies[InitialGeneration](
      cond.get,
      Starall(Seq(i), Nil, Implies(qcond.get, Star(Star(r1.get, r2.get), r3.get)))
    )))
    val out = rw.dispatch(in)

    println(); println()
    println(in)
    println(out)
    println(rw2.dispatch(out))
  }
}
