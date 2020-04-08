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

class SilverProgramFactory[O,Err] extends ProgramFactory[O,Err,Type,Exp,Stmt,
    DomainFunc,DomainAxiom,Prog] with FactoryUtils[O]{

  override def program() : Prog = new Prog 

  override def add_method(p:Prog,o:O,name:String,
      pres:List[Exp],
      posts:List[Exp],
      in:List[Triple[O,String,Type]],
      out:List[Triple[O,String,Type]],
      local:List[Triple[O,String,Type]],
      body:Stmt) {
    
    // TODO : not quite sure if the method body 'body' and the 'locals' are currently handled like this..
    val b = if (body==null) None else Some(Seqn(Seq(body), to_decls(o,local))(NoPosition, new OriginInfo(o), NoTrafos))
    
    p.methods.add(Method(
      name, // method name
      to_decls(o, in), // list of arguments
      to_decls(o, out), // list of return values
      pres.asScala, // list of preconditions
      posts.asScala, // list of postconditions
      b // method body
    )(NoPosition,new OriginInfo(o), NoTrafos))
  }
  
  override def add_field(p:Prog,o:O,name:String,t:Type)={
    p.fields.add(Field(name,t)(NoPosition,new OriginInfo(o,Seq(" a field "))))
  }
  
  override def add_predicate(p:Prog,o:O,name:String,args:List[Triple[O,String,Type]],body:Exp)={
    val b=if(body==null) None else Some(body)
    p.predicates.add(Predicate(name,to_decls(o,args),b)(NoPosition,new OriginInfo(o)))
  }
  
  override def add_function(p:Prog,o:O,name:String,args:List[Triple[O,String,Type]],t:Type,pres:List[Exp],posts:List[Exp],body:Exp)={
    val b = if (body==null) None else Some(body)
    
    p.functions.add(Function(
      name, // function name
      to_decls(o, args), // argument declarations
      t, // function type
      pres.asScala, // sequence of preconditions
      posts.asScala, // sequence of postconditions
      b // function body
    )(NoPosition, new OriginInfo(o), NoTrafos))
  }
  
  override def dfunc(o:O,name:String,args:List[Triple[O,String,Type]],t:Type,domain:String,unique:Boolean)={
    DomainFunc(name,to_decls(o,args),t,unique)(NoPosition,new OriginInfo(o),domain)
  }
  
  override def daxiom(o:O,name:String,expr:Exp,domain:String)={
    DomainAxiom(name,expr)(NoPosition,new OriginInfo(o),domain)
  }
  
  override def add_adt(p:Prog,o:O,name:String,funcs:List[DomainFunc],axioms:List[DomainAxiom],pars:List[String])={
    val args=pars.asScala map {
      d => viper.silver.ast.TypeVar(d)
    }
    p.domains.add(Domain(name,funcs.asScala,axioms.asScala,args)(NoPosition,new OriginInfo(o)));
  }
  
  override def parse_program(x$1: String): viper.api.Prog = {
    Parser.parse_sil(x$1)
  }
  
   
  private def get_info[OO](x:Info,y:Position,f:OriginFactory[OO]):OO={
    x match {
      case in: OriginInfo[OO]@unchecked => {
        in.loc
      }
      case Synthesized =>
        f.message("generated origin")
      case _ => y match {
        case SourcePosition(file,start,tmp) =>
          tmp match {
            case None => f.file(file.toString(),start.line,start.column)
            case Some(end) =>
              if (file==null){
                f.message("null origin");
              } else {
                f.file(file.toString(),start.line,start.column,end.line,end.column)
              }
          }
        case _ => null.asInstanceOf[OO]
      }
    }
  }
  
  private def filter_local_decls(xs : Seq[Declaration]) : Seq[LocalVarDecl] = xs.collect {
    case decl : LocalVarDecl => decl
  }

  override def convert [Err2, T2, E2, S2, DFunc2, DAxiom2, P2](
      api:ViperAPI[O,Err2,T2,E2,S2,DFunc2,DAxiom2,P2], in_prog: viper.api.Prog): P2 = {
    val out_prog = api.prog.program()
    in_prog.domains.asScala.toList foreach {
      d => {
        val o=get_info(d.info,d.pos,api.origin)
        val name=d.name
        val functions:java.util.List[DFunc2]=(d.functions map {
          x => {
            val o=get_info(x.info,x.pos,api.origin)
            val pars=map_decls(api, x.formalArgs)
            val res=map_type(api,x.typ)
            api.prog.dfunc(o,x.name,pars,res,d.name, x.unique)
          }
        }).asJava
        val axioms:java.util.List[DAxiom2]=d.axioms.map {
          x => api.prog.daxiom(o,x.name,map_expr(api,x.exp),d.name)
        }.asJava
        val vars=(d.typVars map { x => x.name } ).asJava
        api.prog.add_adt(out_prog,o,name,functions,axioms,vars)
      }
    }
    in_prog.fields.asScala.toList foreach {
      x => api.prog.add_field(out_prog,get_info(x.info,x.pos,api.origin),x.name,map_type(api,x.typ))
    }
    in_prog.functions.asScala.toList foreach {
      m => {
        val body : E2 = m.body match {
          case None => null.asInstanceOf[E2]
          case Some(e) => map_expr(api,e)
        }
         api.prog.add_function(
            out_prog,
            get_info(m.info,m.pos,api.origin),
            m.name,
            map_decls(api,m.formalArgs),
            map_type(api,m.typ),
            map_expr(api,m.pres),
            map_expr(api,m.posts),
            body)
     }   
    }
    in_prog.methods.asScala.toList foreach {
      m : Method => {
        // TODO : not quite sure if the local declarations are handled correctly like this...
        val body : Seqn = m.body match {
          case None => Seqn(Seq[Stmt](), Seq[Declaration]())(m.pos, m.info, m.errT) // empty body
          case Some(b) => b
        }
        
        api.prog.add_method(
          out_prog, // program
          get_info(m.info,m.pos,api.origin), // origin
          m.name, // method name
          map_expr(api,m.pres), // method preconditions
          map_expr(api,m.posts), // method postconditions
          map_decls(api,m.formalArgs), // input argument declarations
          map_decls(api,m.formalReturns), // output argument declarations (i.e. return values)
          map_decls(api, filter_local_decls(body.scopedDecls)), // list of local variables
          map_stat(api,body) // method body
        ) 
      } 
    }
    in_prog.predicates.asScala.toList foreach {
      m => {
        val body : E2 = m.body match {
          case None => null.asInstanceOf[E2]
          case Some(e) => map_expr(api,e)
        }
        api.prog.add_predicate(
            out_prog,
            get_info(m.info,m.pos,api.origin),
            m.name,
            map_decls(api,m.formalArgs),
            body)
      } 
    }
    out_prog
  }
  
  private def map_stats[Err2, T2, E2, S2, DFunc2, DAxiom2, P2](
      verifier:viper.api.ViperAPI[O,Err2,T2,E2,S2,DFunc2,DAxiom2,P2],
      stats:Seq[Stmt]):List[S2]={
    stats.map {
      e => map_stat(verifier,e)
    }.asJava
  }
  
  private def map_stat[Err2, T2, E2, S2, DFunc2, DAxiom2, P2](
      api:viper.api.ViperAPI[O,Err2,T2,E2,S2,DFunc2,DAxiom2,P2],
      S:Stmt):S2={
     val o=get_info(S.info,S.pos,api.origin)
     S match {
       case Seqn(s, _) => api.stat.block(o, map_stats(api, s)) // TODO : the second argument is now ignored
       case Assert(e) => api.stat.assert_(o,map_expr(api,e))
       case LocalVarAssign(v,e) => api.stat.assignment(o, map_expr(api,v), map_expr(api,e))
       case FieldAssign(v,e) => api.stat.assignment(o, map_expr(api,v), map_expr(api,e))
       case While(c, invs, body) =>
         api.stat.while_loop(o,map_expr(api,c),
             map_expr(api, invs),
             map_decls(api, filter_local_decls(body.scopedDecls)),
             map_stat(api, body))
       case Fold(e) => api.stat.fold(o,map_expr(api,e))
       case Unfold(e) => api.stat.unfold(o,map_expr(api,e))
       case MethodCall(m,in,out) => api.stat.method_call(o,m,
           map_expr(api,in),
           map_expr(api,out),
           null,
           null
       )
       case Fresh(ps) => api.stat.fresh(o,map_expr(api,ps))
       case Constraining(ps,body) => api.stat.constraining(o,map_expr(api,ps),map_stat(api,body))
       case Exhale(e) => api.stat.exhale(o,map_expr(api,e))
       case Goto(e) => api.stat.goto_(o,e)
       case If(c, s1, s2) => api.stat.if_then_else(o,
           map_expr(api,c),map_stat(api,s1),map_stat(api,s2))
       case Inhale(e) => api.stat.inhale(o,map_expr(api,e))
       case Label(e,invs) => api.stat.label(o,e,invs.asInstanceOf[Seq[E2]].asJava)
       case NewStmt(v, fs) => {
         val names=fs map {
           x => x.name
         }
         val types=fs map {
           x => map_type(api,x.typ)
         }
         api.stat.new_object(o,map_expr(api,v),names.asJava,types.asJava)
       }
       
       // TODO implement these
       case LocalVarDeclStmt(e) =>
         throw new Error("'local-var-decl-stmt' not implemented");

       case Apply(_) =>
         throw new Error("apply not implemented");
       case Package(_, _) =>
         throw new Error("package not implemented");
       case Assume(_) =>
         throw new Error("assume not implemented")
       case _: ExtensionStmt =>
         throw new Error("non-explicit extension statements are not supported")
     }
  }

  private def map_expr[Err2, T2, E2, S2, DFunc2, DAxiom2, P2](
      verifier:viper.api.ViperAPI[O,Err2,T2,E2,S2,DFunc2,DAxiom2,P2],
      exps:Seq[Exp]):List[E2]={
    exps.map {
      e => map_expr(verifier,e)
    }.asJava
  }
 
  private def map_expr[Err2, T2, E2, S2, DFunc2, DAxiom2, P2](
      v:viper.api.ViperAPI[O,Err2,T2,E2,S2,DFunc2,DAxiom2,P2],
      exp:Exp):E2={
     val ve=v.expr
     val o=get_info(exp.info,exp.pos,v.origin)
     def m(e:Exp)=map_expr(v,e)
     def t(t:Type)=map_type(v,t)
     //val tt=map_type(v,exp.typ)
     exp match {
       case Let(x,e1,e2) => ve.let(o,x.name,t(x.typ),m(e1),m(e2))
       case AnySetCardinality(e) => ve.size(o,map_expr(v,e))
       case AnySetContains(e1,e2) => ve.any_set_contains(o,m(e1),m(e2))
       case AnySetMinus(e1,e2) => ve.any_set_minus(o,m(e1),m(e2))
       case AnySetUnion(e1,e2) => ve.any_set_union(o,m(e1),m(e2))
       case AnySetIntersection(e1,e2) => ve.any_set_intersection(o,m(e1),m(e2))
       case Result(t) => ve.result(o,map_type(v,t));
       case LocalVar(n,t) => ve.local_name(o,n,map_type(v,t))
       case IntLit(i) => ve.Constant(o,i.toInt)
       case TrueLit() => ve.Constant(o,true)
       case FalseLit() => ve.Constant(o,false)
       case NullLit() => ve.null_(o)
       case FractionalPerm(e1,e2) => ve.frac(o,map_expr(v,e1),map_expr(v,e2))
       case EqCmp(e1,e2) => ve.eq(o,map_expr(v,e1),map_expr(v,e2))
       case NeCmp(e1,e2) => ve.neq(o,map_expr(v,e1),map_expr(v,e2))
       case GtCmp(e1,e2) => ve.gt(o,map_expr(v,e1),map_expr(v,e2))
       case PermLtCmp(e1, e2) => ve.lt(o, map_expr(v, e1), map_expr(v, e2))
       case PermLeCmp(e1, e2) => ve.lte(o, map_expr(v, e1), map_expr(v, e2))
       case PermGeCmp(e1, e2) => ve.gte(o, map_expr(v, e1), map_expr(v, e2))
       case PermGtCmp(e1,e2) => ve.gt(o,map_expr(v,e1),map_expr(v,e2))
       case LtCmp(e1,e2) => ve.lt(o,map_expr(v,e1),map_expr(v,e2))
       case GeCmp(e1,e2) => ve.gte(o,map_expr(v,e1),map_expr(v,e2))
       case LeCmp(e1,e2) => ve.lte(o,map_expr(v,e1),map_expr(v,e2))
       case Mul(e1,e2) => ve.mult(o,map_expr(v,e1),map_expr(v,e2))
       case Mod(e1,e2) => ve.mod(o,map_expr(v,e1),map_expr(v,e2))
       case PermDiv(e1,e2) => ve.div(o,map_expr(v,e1),map_expr(v,e2))
       case PermAdd(e1,e2) => ve.add(o,map_expr(v,e1),map_expr(v,e2))
       case PermMul(e1,e2) => ve.mult(o,map_expr(v,e1),map_expr(v,e2))
       case PermSub(e1,e2) => ve.sub(o,map_expr(v,e1),map_expr(v,e2))
       case IntPermMul(e1,e2) => ve.mult(o,map_expr(v,e1),map_expr(v,e2));
       case Div(e1,e2) => ve.floor_div(o,map_expr(v,e1),map_expr(v,e2))
       case Add(e1,e2) => ve.add(o,map_expr(v,e1),map_expr(v,e2))
       case Sub(e1,e2) => ve.sub(o,map_expr(v,e1),map_expr(v,e2))
       case Minus(e) => ve.neg(o,map_expr(v,e))
       case FieldAccess(e,Field(name,t)) =>
         ve.FieldAccess(o,map_expr(v,e),name,map_type(v,t))
       case FieldAccessPredicate(e1,e2) =>
         ve.field_access(o,map_expr(v,e1),map_expr(v,e2))
       case FullPerm() => ve.write_perm(o)
       case WildcardPerm() => ve.read_perm(o)
       case NoPerm() => ve.no_perm(o)
       case CurrentPerm(e) => ve.current_perm(o,map_expr(v,e))
       case Not(e) => ve.not(o,map_expr(v,e))
       case And(e1,e2) => ve.and(o,map_expr(v,e1),map_expr(v,e2))
       case Or(e1,e2) => ve.or(o,map_expr(v,e1),map_expr(v,e2))
       case Implies(e1,e2) => ve.implies(o,map_expr(v,e1),map_expr(v,e2))
       case FuncApp(name,args) => ve.function_call(o,name,map_expr(v,args),
           map_type(v, exp.asInstanceOf[FuncApp].typ))
       case CondExp(e1,e2,e3) => ve.cond(o,map_expr(v,e1),map_expr(v,e2),map_expr(v,e3))
       case Unfolding(e1,e2) => ve.unfolding_in(o,map_expr(v,e1),map_expr(v,e2))
       case PredicateAccessPredicate(e1,e2) =>  ve.scale_access(o,map_expr(v,e1),map_expr(v,e2))
       case PredicateAccess(args,name) => ve.predicate_call(o,name,map_expr(v,args))
       case DomainFuncApp(name,args,typemap) =>
         ve.domain_call(o,name,map_expr(v,args), map_type_map(v,typemap),
             map_type(v, exp.asInstanceOf[DomainFuncApp].typ),
             exp.asInstanceOf[DomainFuncApp].domainName)
       case Forall(vars,triggers,e) =>
         val trigs : List[List[E2]] = (triggers map {
           x:Trigger => map_expr(v,x.exps)
         }).asJava
         ve.forall(o,map_decls(v,vars),trigs,map_expr(v,e))
       case EmptyMultiset(t) =>
         ve.explicit_bag(o,map_type(v,t),Seq().asJava)
       case EmptySeq(t) =>
         ve.explicit_seq(o,map_type(v,t),Seq().asJava)
       case EmptySet(t) =>
         ve.explicit_set(o,map_type(v,t),Seq().asJava)
       case ExplicitMultiset(xs) =>
         ve.explicit_bag(o,map_type(v,xs.head.typ),map_expr(v,xs))
       case ExplicitSeq(xs) =>
         ve.explicit_seq(o,map_type(v,xs.head.typ),map_expr(v,xs))
       case ExplicitSet(xs) =>
         ve.explicit_set(o,map_type(v,xs.head.typ),map_expr(v,xs))
       case SeqAppend(xs,ys) =>
         ve.append(o, map_expr(v,xs),map_expr(v,ys))
       case SeqLength(xs) =>
         ve.size(o,map_expr(v,xs))
       case SeqDrop(xs,n) =>
         ve.drop(o,m(xs),m(n))
       case SeqTake(xs,n) =>
         ve.take(o,m(xs),m(n))
       case SeqIndex(xs,n) => ve.index(o,m(xs),m(n))
       case x => throw new Error("missing case in map expr "+x.getClass())
     }
  }
  
  private def map_type_map[Err2, T2, E2, S2, DFunc2, DAxiom2, P2](
      verifier:viper.api.ViperAPI[O,Err2,T2,E2,S2,DFunc2,DAxiom2,P2],
       typemap: Map[TypeVar, Type]):java.util.Map[String,T2]
  ={
    (typemap map { case (k,v) => (k.toString(),map_type(verifier,v)) }).asJava 
  }

  
  private def map_decls[Err2, T2, E2, S2, DFunc2, DAxiom2, P2](
      verifier:viper.api.ViperAPI[O,Err2,T2,E2,S2,DFunc2,DAxiom2,P2],
      decls:Seq[LocalVarDecl])={
    decls.map {
      d =>
        val o=get_info(d.info,d.pos,verifier.origin)
        new viper.api.Triple[O,String,T2](o,d.name,map_type(verifier,d.typ))
    }.asJava
  }

  private def map_type[Err2, T2, E2, S2, DFunc2, DAxiom2, P2](
      verifier:viper.api.ViperAPI[O,Err2,T2,E2,S2,DFunc2,DAxiom2,P2],
      t:Type):T2={
    t match {
      case viper.silver.ast.Int => verifier._type.Int()
      case viper.silver.ast.Perm => verifier._type.Perm()
      case viper.silver.ast.Ref => verifier._type.Ref()
      case viper.silver.ast.Bool => verifier._type.Bool()
      case DomainType(name, args) => verifier._type.domain_type(name,map_type_map(verifier,args))
      case MultisetType(t) => verifier._type.Bag(map_type(verifier,t))
      case SeqType(t) => verifier._type.List(map_type(verifier,t))
      case SetType(t) => verifier._type.Set(map_type(verifier,t))
      case TypeVar(name) => verifier._type.type_var(name)
      case InternalType => throw new Error("cannot support internal type")
      case Wand => throw new Error("cannot support magic wand")
    }
  
  }
}

object Parser extends viper.silver.frontend.SilFrontend {
  private var silicon: viper.silver.verifier.NoVerifier = new viper.silver.verifier.NoVerifier

  def parse_sil(name:String) = {
    configureVerifier(Nil);
    init(silicon)
    reset(java.nio.file.Paths.get(name))
    parsing()
    semanticAnalysis()
    translation()
    _program match {
      case Some(Program(domains,fields,functions,predicates,methods)) => 
        val prog=new Prog();
          prog.domains.addAll(domains.asJava)
          prog.fields.addAll(fields.asJava)
          prog.functions.addAll(functions.asJava)
          prog.predicates.addAll(predicates.asJava)
          prog.methods.addAll(methods.asJava)
        prog;
      case _ => throw new Error("empty file");
    }
  }

  def configureVerifier(args: Seq[String]): viper.silver.frontend.SilFrontendConfig = {
    null
  }
  
  def createVerifier(fullCmd: String): viper.silver.verifier.Verifier = {
    new viper.silver.verifier.NoVerifier
  }
  
}


