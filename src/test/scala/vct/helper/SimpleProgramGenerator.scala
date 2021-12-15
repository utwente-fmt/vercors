package vct.helper

import vct.col.ast._
import vct.col.origin._
import vct.col.util.AstBuildHelpers._

import java.nio.file.Paths

object SimpleProgramGenerator {

  def generateSimpleInputOrigin(): Origin = {
    DiagnosticOrigin
  }

  def generateSimpleApplicableContract[G]()(implicit origin: Origin = generateSimpleInputOrigin()): ApplicableContract[G] ={
    ApplicableContract(BooleanValue(value = true),BooleanValue(value = true),BooleanValue(value = true),Seq(),Seq(),Seq())
  }

  def generateProgramWithSingleClassAndSingleMethod[G](body: Statement[G])(implicit origin: Origin = generateSimpleInputOrigin()): Program[G] ={
    val contract1 = generateSimpleApplicableContract[G]()
    val blame1 = origin
    val method1 = new InstanceMethod(TVoid(), Nil, Nil, Nil, Option(body), contract1)(blame1)
    val classNode1 = new Class(Seq(method1), Nil, tt)
    Program(Seq(classNode1), None)(DiagnosticOrigin)
  }

}
