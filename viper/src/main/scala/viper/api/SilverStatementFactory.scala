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


class SilverStatementFactory[O] extends StatementFactory[O,Type,Exp,Stmt] with FactoryUtils[O] {
  
  override def if_then_else(o:O, c:Exp, s1:Stmt, s2:Stmt) : Stmt = {
    // TODO : not quite sure if the declarations are currently handled like this..
    val s_true = Seqn(Seq(s1), Seq())(s1.pos, s1.info, s1.errT)
    val s_false = Seqn(Seq(s2), Seq())(s2.pos, s2.info, s2.errT)
    If(c, s_true, s_false)(NoPosition, new OriginInfo(o))
  }
  
  override def method_call(o:O,name:String,in_args:List[Exp],out_args:List[Exp],
      pars:List[Triple[O,String,Type]],rets:List[Triple[O,String,Type]]) : Stmt = {
    
    val m = add(Method(
      name, // method name
      to_decls(o, pars), // list of arguments
      to_decls(o, rets), // list of return values
      Nil, // preconditions
      Nil, // postconditions
      None // method body
    )_, o)
    
    val outs = out_args.asScala.map {
      x => x.asInstanceOf[LocalVar]
    }
    
    add(MethodCall(m, in_args.asScala, outs)_, o)
  } 
  
  override def new_object(o:O, v:Exp,names:List[String],types:List[Type]):Stmt={
    val vs = (names.asScala zip types.asScala).map { a => a match { case (n,t) => Field(n,t)(NoPosition,new OriginInfo(o)) } }
    NewStmt(v.asInstanceOf[LocalVar],vs)(NoPosition,new OriginInfo(o))
  }
  override def inhale(o:O, e:Exp) : Stmt = Inhale(e)(NoPosition,new OriginInfo(o))
  override def exhale(o:O, e:Exp) : Stmt = Exhale(e)(NoPosition,new OriginInfo(o))
  override def assert_(o:O, e:Exp) : Stmt = Assert(e)(NoPosition,new OriginInfo(o,Seq("assert")))
  override def refute(o:O, e:Exp) : Stmt = {
    Assert(Not(e)(NoPosition,new RefuteInfo(o)))(NoPosition,new OriginInfo(o,Seq("refute")))
  }
  
  override def fold(o:O, e:Exp) : Stmt = Fold(e.asInstanceOf[PredicateAccessPredicate])(NoPosition,new OriginInfo(o))
  override def unfold(o:O, e:Exp) : Stmt = Unfold(e.asInstanceOf[PredicateAccessPredicate])(NoPosition,new OriginInfo(o))
  override def goto_(o:O, l:String) : Stmt = Goto(l)(NoPosition,new OriginInfo(o))
  override def label(o:O, l:String, invs:List[Exp]) : Stmt = Label(l, invs.asScala)(NoPosition, new OriginInfo(o))
  override def assignment(o:O,loc:Exp,v:Exp) : Stmt = {
    loc match {
        case l : FieldAccess =>
          new FieldAssign(l,v)(NoPosition,new OriginInfo(o))
        case l : LocalVar =>
          new LocalVarAssign(l,v)(NoPosition,new OriginInfo(o))
        case l => 
          throw new Error("failed to map ASTNode of type "+l.getClass());
      }
  }

  override def while_loop(o:O,cond:Exp,inv:List[Exp],locals:List[Triple[O,String,Type]],body:Stmt):Stmt = {
    val locs=locals.asScala.toArray.map {
      d => LocalVarDecl(d.v2,d.v3)(NoPosition,new OriginInfo(d.v1))
    }

    // TODO not quite sure if the local variable declarations are handled correctly now
    val b : Seqn = body match {
      case null => Seqn(Seq(), locs)()
      case s => Seqn(Seq(s), locs)(s.pos, s.info, s.errT)
    }
    
    While(
      cond, // condition expression
      inv.asScala, // sequence of loop invariants
      b // loop body
    )(NoPosition, new OriginInfo(o))
  }
  
  // TODO not sure if 'scopedDecls' (of the class 'Seqn') is properly handled here
  override def block(o:O, stats:List[Stmt]): Stmt =
    Seqn(stats.asScala, Seq())(NoPosition, new OriginInfo(o))
  
  def constraining(o:O, ps:List[Exp], body:Stmt) : Stmt = {
    val vars = ps.asScala.map {
      x => x.asInstanceOf[LocalVar]
    }
    
    // TODO not sure if the local variables declarations are still handled properly...
    val b : Seqn = body match {
      case null => Seqn(Seq(), Seq())()
      case s => Seqn(Seq(s), Seq())(s.pos, s.info, s.errT)
    }
    
    Constraining(vars, b)(NoPosition, new OriginInfo(o))
  }
  
  def fresh(o: O,ps: List[Exp]): Stmt = {
    val vars=ps.asScala.map {
      x => x.asInstanceOf[LocalVar]
    }
    Fresh(vars)(NoPosition,new OriginInfo(o))
  }
}

