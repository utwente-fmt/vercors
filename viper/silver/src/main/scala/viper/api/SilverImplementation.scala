package viper.api

import viper.silver.ast._
import scala.collection.JavaConverters._
import scala.collection.JavaConverters._
import viper.silver.verifier.{Failure, Success, AbortedExceptionally, VerificationError}
import java.util.List
import java.util.Properties
import java.util.SortedMap
import scala.math.BigInt.int2bigInt
import viper.silver.ast.SeqAppend
import java.nio.file.Path
import viper.silver.parser.PLocalVarDecl
import scala.collection.mutable.WrappedArray

class SilverImplementation[O,Err](o:OriginFactory[O])
  extends viper.api.ViperAPI[O,Err,Type,Exp,Stmt,DomainFunc,DomainAxiom,Prog](o,
        new SilverTypeFactory,
        new SilverExpressionFactory[O],
        new SilverStatementFactory[O],
        new SilverProgramFactory[O,Err]) {
  
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
 
  override def verify(tool_home:Path,settings:Properties,prog:Prog,reachable:java.util.Set[O],
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
    Reachable.reachable.clear()
    val verifier=createVerifier(tool_home,settings)
    //println("verifier: "+ verifier);
    println("running verify");
    val res = verifier.verify(program)
    println("finished verify");
    //println("verifier output: "+ res);
    res match {
      case Success =>
        println("Success!")
      case Failure(errors) =>
        println("Errors! ("+errors.length+")")
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
                  //show("offending node's info", in.info)
                  in.info match {
                    case in: OriginInfo[O] => {
                      val loc=in.asInstanceOf[OriginInfo[O]].loc
                      //report.add(error_factory.generic_error(loc,err))
                      new viper.api.ViperErrorImpl[O](loc,err)
                    }
                    case _ => {
                      new viper.api.ViperErrorImpl[O](in.pos+": "+err)
                      //throw new Error("info is not an origin!")
                    }
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
                      val loc=in.asInstanceOf[OriginInfo[O]].loc
                      
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
              show("caused by ", ae.cause)
              report.add(new ViperErrorImpl(null.asInstanceOf[O],ae.fullId));
            }
            case x => {
              report.add(new ViperErrorImpl(null.asInstanceOf[O],x.fullId));
            }
              
          }
         }
       
    }
    Reachable.reachable.map(
      o => reachable.add(o.asInstanceOf[OriginInfo[O]].loc)
    )
    report
  }
 
  // Members declared in viper.api.SilverImplementation
  def createVerifier(tool_home: java.nio.file.Path,settings: java.util.Properties): 
   viper.silver.verifier.Verifier = {
     new viper.silver.verifier.NoVerifier
  }
  
}


