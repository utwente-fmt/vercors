package vct.helper

import vct.col.ast.Constant._
import vct.col.ast.{ApplicableContract, Class, FileOrigin, InputOrigin, InstanceMethod, Origin, Program, Statement, TVoid}

import java.nio.file.Paths

object SimpleProgramGenerator {

  def generateSimpleInputOrigin(): InputOrigin ={
    FileOrigin(Paths.get(""), 1, 1, 1, 1)
  }

  def generateSimpleApplicableContract()(implicit origin: Origin = generateSimpleInputOrigin()): ApplicableContract ={
    ApplicableContract(BooleanValue(value = true),BooleanValue(value = true),BooleanValue(value = true),Seq(),Seq(),Seq())
  }

  def generateProgramWithSingleClassAndSingleMethod(body: Statement)(implicit origin: InputOrigin = generateSimpleInputOrigin()): Program ={
    val contract1 = generateSimpleApplicableContract
    val blame1 = origin
    val method1 = new InstanceMethod(TVoid(), Seq(), Seq(), Option(body), contract1)(blame1)
    val classNode1 = new Class(Seq(method1))
    Program(Seq(classNode1))
  }

}
