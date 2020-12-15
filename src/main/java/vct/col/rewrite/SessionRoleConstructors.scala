package vct.col.rewrite

import hre.ast.MessageOrigin
import hre.lang.System.Output
import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{MethodInvokation, NameExpression, NameExpressionKind, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.util.SessionRolesAndMain

import scala.collection.JavaConversions._

class SessionRoleConstructors(override val source: ProgramUnit)  extends AbstractRewriter(null, true) {

  var chanMap : Map[String,Set[(String,Boolean)]] = Map() // A Map threadName -> {(chanName,isWriteValueCall)}

  def getConstructors() = {
    for(entry <- source.get()) {
      entry match {
        case c : ASTClass => {
          if(SessionRolesAndMain.isThreadClassName(c.name)) {
            val chans = getChansFromRunMethod(c)
            chanMap += (c.name -> chans)
            val chanFields = chans.map(chan => create.field_decl(new MessageOrigin("Generated Channel field " + chan._1 + " of class " + c.name),chan._1,SessionRolesAndMain.getChanClass()))
            chanFields.foreach(c.add_dynamic(_))
          }
        }
      }
    }
    this.rewriteAll()
  }

  private def getChansFromRunMethod(c : ASTClass) : Set[(String,Boolean)] = {
    c.methods().find(_.name == SessionRolesAndMain.runMethodName) match {
      case Some(runMethod) => {
        val chanMethods = getChansFromBlockStateMent(runMethod.getBody)
        chanMethods.filter(m => m.`object` match {
          case n: NameExpression => true
          case _ => false
        }).map(m => {
          (m.`object`.asInstanceOf[NameExpression].name, m.method == SessionRolesAndMain.chanWrite)
        })
      }
      case None => Fail("Session Fail: Thread has no 'run' method"); Set()
    }
  }

  private def getChansFromBlockStateMent(block : ASTNode) : Set[MethodInvokation] = {
    var methods : Set[MethodInvokation] = Set()
    block match {
      case b : BlockStatement =>
        for(stat <- b.getStatements) {
          stat match{
            case l: LoopStatement => methods = methods ++ getChansFromBlockStateMent(l.getBody)
            case p: ParallelRegion => p.blocks.map(b => methods = methods ++ getChansFromBlockStateMent(b.block))
            case m: MethodInvokation =>
              if (m.method == SessionRolesAndMain.chanRead || m.method == SessionRolesAndMain.chanWrite) {
                methods = methods ++ List(m)
              }
            case _ => //nothing
        }
      }
      case _ => Fail("Session Fail: expected BlockStatement");
    }
    methods
  }

  override def visit(m : Method) = {
    if(m.kind == Method.Kind.Constructor && SessionRolesAndMain.isThreadClassName(m.name)) {
      create.enter()
      create.setOrigin(new MessageOrigin("Generated constructor " + m.name))
      val chans = chanMap.get(m.name).get
      val args : Set[DeclarationStatement] = chans.map(chan => create.field_decl(getArgChan(chan._1),SessionRolesAndMain.getChanClass()))
      val statements : Set[ASTNode] = chans.map(chan => create.assignment(create.name(NameExpressionKind.Unresolved,null,chan._1),create.name(NameExpressionKind.Unresolved,null,getArgChan(chan._1))))
      val body : BlockStatement = create.block(statements.toArray:_*)
      val myNewMethod = create.method_kind(m.kind,m.getReturnType,getRoleConstructorContract(chans),m.name,args.toArray,body)
      create.leave()
      result = myNewMethod
    } else {
      super.visit(m)
    }
  }

  private def getRoleConstructorContract(chans : Set[(String,Boolean)]) = {
    val contract = new ContractBuilder()
    val reqsentrecvd = chans.map(chan => getChanFieldPerm(getArgChan(chan._1),chan._2))
    val reqnull = chans.map(chan => create.expression(StandardOperator.EQ,create.name(NameExpressionKind.Unresolved,null,getArgChan(chan._1)),
      create.reserved_name(ASTReserved.Null)))
    val ensperm = chans.map(chan => create.expression(StandardOperator.Perm,create.name(NameExpressionKind.Unresolved,null,chan._1),getHalfFraction()))
    val ensneq = chans.map(chan => create.expression(StandardOperator.NEQ,create.name(NameExpressionKind.Unresolved,null,chan._1),
      create.name(NameExpressionKind.Unresolved,null,getArgChan(chan._1))))
    val enssentrecvd = chans.map(chan => getChanFieldPerm(chan._1,chan._2))
    reqsentrecvd.foreach(contract.requires(_))
    reqnull.foreach(contract.requires(_))
    ensperm.foreach(contract.ensures(_))
    ensneq.foreach(contract.ensures(_))
    enssentrecvd.foreach(contract.ensures(_))
    contract.getContract
  }

  private def getChanFieldPerm(chanName : String, isWrite : Boolean) : ASTNode = {
    val arg1 = create.dereference(create.name(NameExpressionKind.Unresolved,null,chanName), if(isWrite) "sent" else "recvd")
    create.expression(StandardOperator.Perm,arg1,getHalfFraction())
  }

  private def getHalfFraction() = create.expression(StandardOperator.Div,create.constant(1),create.constant(2))

  def getArgChan(chan : String) = chan + "Arg"

}
