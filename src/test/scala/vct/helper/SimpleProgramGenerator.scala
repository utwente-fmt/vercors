package vct.helper

import vct.col.ast.Constant._
import vct.col.ast._
import vct.col.origin._

import java.nio.file.Paths

object SimpleProgramGenerator {

  def generateSimpleInputOrigin(): Origin = {
    DiagnosticOrigin
  }

  def generateSimpleApplicableContract()(implicit origin: Origin = generateSimpleInputOrigin()): ApplicableContract ={
    ApplicableContract(BooleanValue(value = true),BooleanValue(value = true),BooleanValue(value = true),Seq(),Seq(),Seq())
  }

  def generateProgramWithSingleClassAndSingleMethod(body: Statement)(implicit origin: Origin = generateSimpleInputOrigin()): Program ={
    val contract1 = generateSimpleApplicableContract()
    val blame1 = origin
    val method1 = new InstanceMethod(TVoid(), Nil, Nil, Nil, Option(body), contract1)(blame1)
    val classNode1 = new Class(Seq(method1), Nil)
    Program(Seq(classNode1))(DiagnosticOrigin)
  }

}
