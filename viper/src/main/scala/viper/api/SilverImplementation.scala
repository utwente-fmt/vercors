package viper.api

import viper.silver.ast._

import scala.collection.JavaConverters._
import scala.collection.JavaConverters._
import viper.silver.verifier.{AbortedExceptionally, Failure, Success, VerificationError}
import java.util.List
import java.util.Properties
import java.util.SortedMap

import scala.math.BigInt.int2bigInt
import viper.silver.ast.SeqAppend
import java.nio.file.Path

import hre.ast.OriginFactory
import viper.silver.parser.PLocalVarDecl

import scala.collection.mutable.WrappedArray
import hre.lang.System.Output

class SilverImplementation[O](o:OriginFactory[O])
  extends viper.api.ViperAPI[O,Type,Exp,Stmt,DomainFunc,DomainAxiom,Prog](o,
        new SilverTypeFactory,
        new SilverExpressionFactory[O],
        new SilverStatementFactory[O],
        new SilverProgramFactory[O]) {
  
  override def write_program(pw:java.io.PrintWriter,prog:Prog):Unit={
    val program = Program(prog.domains.asScala.toList,
              prog.fields.asScala.toList,
              prog.functions.asScala.toList,
              prog.predicates.asScala.toList,
              prog.methods.asScala.toList)()
    pw.write(program.toString())
  }
  
  private def getOrigin(e : Object) : O = e.asInstanceOf[Infoed].info.asInstanceOf[O]
  
 
  private def show(text: String, obj: Any) {
    println(s"$text (${obj.getClass.getSimpleName}): $obj")
  }
  
  private def locFromInfo(in: Info): Option[O] = {
      in match {
      case in: OriginInfo[O] => {
        Some(in.asInstanceOf[OriginInfo[O]].loc)
      }
      case in: ConsInfo => {
        val cin = in.asInstanceOf[ConsInfo]
        locFromInfo(cin.head) match {
          case Some(loc) => Some(loc)
          case None => locFromInfo(cin.tail)
        }
      }
      case _ => None
    }
  }
 
  override def verify(z3Path:Path,z3Settings:Properties,prog:Prog,
      control:VerificationControl[O]) : List[viper.api.ViperError[O]] = {
    val program = Program(prog.domains.asScala.toList,
              prog.fields.asScala.toList,
              prog.functions.asScala.toList,
              prog.predicates.asScala.toList,
              prog.methods.asScala.toList)()
              
    //println("=============\n" + program + "\n=============\n")
    
    Reachable.gonogo = control.asInstanceOf[VerificationControl[Object]];
    
    val detail = Reachable.gonogo.detail();
    
    val report = new java.util.ArrayList[viper.api.ViperError[O]]()
    val verifier=createVerifier(z3Path,z3Settings)
    //println("verifier: "+ verifier);
    //Progress("running verify");
    val res = verifier.verify(program)
    //Progress("finished verify");
    //println("verifier output: "+ res);
    res match {
      case Success =>
        ()
      case Failure(errors) =>
        errors foreach { e =>
          if (detail) show("error", e)
          e match {
            case ve: VerificationError =>
              if (detail) {
                show("offending node", ve.offendingNode)
                show("reason", ve.reason.id);
              }
              val err=ve.fullId
              val error = ve.offendingNode match {
                //ve match {
                 case in: viper.silver.ast.Infoed =>
                  locFromInfo(in.info) match {
                    case Some(loc) => new viper.api.ViperErrorImpl[O](loc,err)
                    case None => new viper.api.ViperErrorImpl[O](in.pos+": "+err)
                  }
                case _ =>
                  new viper.api.ViperErrorImpl[O](err)
              }
              report.add(error);
              val because="because of "+ve.reason.id
              ve.reason.offendingNode match {
                //ve match {
                case in: viper.silver.ast.Infoed =>
                  //show("offending node's info", in.info)
                  in.info match {
                    case in: OriginInfo[O] => {
                      val loc = in.loc;
                      //report.add(error_factory.generic_error(loc,err))
                      error.add_extra(loc,because);
                    }
                    case _ => {
                      error.add_extra(in.pos+": "+because)
                      //throw new Error("info is not an origin!")
                    }
                  }
                case _ =>
                  error.add_extra(because)
              }
            case ae : AbortedExceptionally =>{
              if (detail) show("caused by ", ae.cause)
              report.add(new ViperErrorImpl(null.asInstanceOf[O],ae.fullId));
            }
            case x => {
              report.add(new ViperErrorImpl(null.asInstanceOf[O],x.fullId));
            }
              
          }
         }
       
    }

    report
  }
 
  // Members declared in viper.api.SilverImplementation
  def createVerifier(z3Path: java.nio.file.Path, z3Settings: java.util.Properties): 
   viper.silver.verifier.Verifier = {
     new viper.silver.verifier.NoVerifier
  }
  
}


