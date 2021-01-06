package vct.col.rewrite

import hre.ast.MessageOrigin
import hre.lang.System.Output
import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.{MethodInvokation, NameExpression, NameExpressionKind, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.util.SessionChannel
import vct.col.util.SessionUtil.{chanRead, chanWrite, getChanClass, isThreadClassName, runMethodName}

import scala.collection.JavaConversions._

class SessionThreadConstructors(override val source: ProgramUnit)  extends AbstractRewriter(null, true) {

  var chanMap : Map[String,Set[SessionChannel]] = Map() // A Map threadName -> {(chanName,isWriteValueCall)}

  def getConstructors() = {
    for(entry <- source.get()) {
      entry match {
        case c : ASTClass => {
          if(isThreadClassName(c.name)) {
            val chans = getChansFromRunMethod(c)
            chanMap += (c.name -> chans)
            val chanFields = chans.map(chan => create.field_decl(new MessageOrigin("Generated Channel field " + chan.channel + " of class " + c.name),chan.channel,getChanClass()))
            chanFields.foreach(c.add_dynamic(_))
          }
        }
      }
    }
  }

  private def getChansFromRunMethod(c : ASTClass) : Set[SessionChannel] = {
    c.methods().find(_.name == runMethodName) match {
      case Some(runMethod) => {
        val chanMethods = getChansFromBlockStateMent(runMethod.getBody)
        chanMethods.filter(m => m.`object` match {
          case n: NameExpression => true
          case _ => false
        }).map(m => {
          new SessionChannel(m.`object`.asInstanceOf[NameExpression].name, m.method == chanWrite)
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
              if (m.method == chanRead || m.method == chanWrite) {
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
    if(m.kind == Method.Kind.Constructor && isThreadClassName(m.name)) {
      create.enter()
      create.setOrigin(new MessageOrigin("Generated constructor " + m.name))
      val chans = chanMap.get(m.name).get
      val args : Set[DeclarationStatement] = chans.map(chan => create.field_decl(chan.getArgChanName(),getChanClass()))
      val statements : Set[ASTNode] = chans.map(chan =>
        create.assignment(create.name(NameExpressionKind.Unresolved,null,chan.channel),
                          create.name(NameExpressionKind.Unresolved,null,chan.getArgChanName())))
      val body : BlockStatement = create.block(statements.toArray:_*)
      val myNewMethod = create.method_kind(m.kind,m.getReturnType,getRoleConstructorContract(chans),m.name,args.toArray,body)
      create.leave()
      result = myNewMethod
    } else {
      super.visit(m)
    }
  }

  private def getRoleConstructorContract(chans : Set[SessionChannel]) = {
    val contract = new ContractBuilder()
    val reqSentRecvd = chans.map(_.getArgChan().getChanFieldPerm(create))
    val reqNotNull = chans.map(chan =>
      create.expression(StandardOperator.NEQ,
        create.name(NameExpressionKind.Unresolved,null,chan.getArgChanName()),
        create.reserved_name(ASTReserved.Null)))
    val ensPerm = chans.map(chan =>
      create.expression(StandardOperator.Perm,
        create.name(NameExpressionKind.Unresolved,null,chan.channel),
        create.reserved_name(ASTReserved.ReadPerm)))
    val ensArgEq = chans.map(chan =>
      create.expression(StandardOperator.EQ,
        create.name(NameExpressionKind.Unresolved,null,chan.channel),
        create.name(NameExpressionKind.Unresolved,null,chan.getArgChanName())))
    val ensSentRecvd = chans.map(_.getChanFieldPerm(create))
    reqSentRecvd.foreach(contract.requires(_))
    reqNotNull.foreach(contract.requires(_))
    ensPerm.foreach(contract.ensures(_))
    ensArgEq.foreach(contract.ensures(_))
    ensSentRecvd.foreach(contract.ensures(_))
    contract.getContract
  }



}
