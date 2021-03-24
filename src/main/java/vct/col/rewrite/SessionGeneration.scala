package vct.col.rewrite

import hre.ast.MessageOrigin
import hre.lang.System.Output
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, PrimitiveType}
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, NameExpressionKind, OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, IfStatement, IfStatementCase, LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTClass, Contract, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}
import vct.col.util.SessionStructureCheck
import vct.col.util.SessionUtil.{barrierAwait, barrierFieldName, chanRead, chanWrite, getArgName, getBarrierClass, getChanClass, getChanName, getNameFromNode, getNamesFromExpression, getThreadClassName, mainClassName, mainMethodName}

import scala.collection.convert.ImplicitConversions.{`collection asJava`, `iterable AsScalaIterable`}

class SessionGeneration(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  private val roleNames : Iterable[String] = SessionStructureCheck.getRoleNames(source)
  private val mainClass = SessionStructureCheck.getMainClass(source)
  private var roleName : String = "Error No Role Name!"

  private var chans : Set[String] = Set()

  def addThreadClasses() : ProgramUnit = {
    source.get().filter(_.name != mainClassName).foreach(target().add(_))
    roleNames.foreach(role => {
      target().add(createThreadClass(role))
    })
    target()
  }

  private def createThreadClass(role : String) = {
    create.enter()
    roleName = role
    chans = Set()
    create.setOrigin(new MessageOrigin("Generated thread class " + roleName))
    val threadName = getThreadClassName(roleName)
    val thread = create.new_class(threadName,null,null)
    val rewMethods = mainClass.methods().filter(_.name != mainMethodName).map(rewrite(_))
    mainClass.fields().forEach(f => if(f.name == roleName) thread.add(rewrite(f)))
    chans.foreach(chan => thread.add_dynamic(create.field_decl(chan,getChanClass())))
    thread.add_dynamic(create.field_decl(barrierFieldName,getBarrierClass()))
    rewMethods.foreach(thread.add)
    create.leave()
    thread
  }

  override def visit(m : Method) : Unit = { //assume ony pre and postconditions
    val c = m.getContract()
    val cb = new ContractBuilder()
    cb.requires(rewrite(c.pre_condition))
    cb.ensures(rewrite(c.post_condition))
    if(m.kind == Method.Kind.Constructor) {
      result = create.method_kind(m.kind,m.getReturnType,cb.getContract,getThreadClassName(roleName),m.getArgs,rewrite(m.getBody))
    } else if(m.kind == Method.Kind.Pure) {
      result = copy_rw.rewrite(m)
    } else {
      result = create.method_kind(m.kind,m.getReturnType,cb.getContract,m.name,m.getArgs,rewrite(m.getBody))
    }
  }

  override def visit(l : LoopStatement) : Unit = { //it is while loop
    val c = l.getContract
    val cb = new ContractBuilder()
    cb.appendInvariant(rewrite(c.invariant))
    result = create.while_loop(rewrite(l.getEntryGuard),rewrite(l.getBody),cb.getContract)
  }

  override def visit(pb : ParallelBlock) : Unit = {
    val c = pb.contract
    val cb = new ContractBuilder()
    cb.requires(rewrite(c.pre_condition))
    cb.ensures(rewrite(c.post_condition))
    result = create.parallel_block(pb.label,cb.getContract,pb.itersJava,rewrite(pb.block),pb.deps)
  }

  override def visit(a : AssignmentStatement) : Unit = {
    val locRole = getNameFromNode(a.location).get
    val expRole = getNamesFromExpression(a.expression)
    if(locRole.name == roleName && (expRole.isEmpty || expRole.size == 1 && expRole.head.name == roleName)) { //it is a normal assignment for roleName
      result = copy_rw.rewrite(a)
    } else if(locRole.name == roleName && expRole.size == 1 && expRole.head.name != roleName) { // it is a read for roleName
      val chan = getChanVar(expRole.head,false)
      chans += chan.name
      result = create.assignment(a.location,create.invokation(chan,null, chanRead))
    } else if(locRole.name != roleName && expRole.size == 1 && expRole.head.name == roleName){ // it is a write for roleName
      val chan = getChanVar(locRole,true)
      chans += chan.name
      result = create.invokation(chan, null, chanWrite, a.expression)
    } else if(locRole.name != roleName && (expRole.isEmpty || expRole.size == 1 && expRole.head.name != roleName)) {
        //remove a
    } else {
      Fail("Session Fail: assignment %s is no session assignment! ", a.toString)
    }
  }

  override def visit(e : OperatorExpression) : Unit = {
    if(e.operator == StandardOperator.Star || e.operator == StandardOperator.And) {
      result = create.expression(e.operator,rewrite(e.first),rewrite(e.second))
    } else rewriteExpression(e)
  }

  override def visit(m : MethodInvokation) : Unit = {
    m.getParent match {
      case b :BlockStatement => //it is a statement
        if(isSingleRoleNameExpression(m, roleNames))
          copy_rw.rewrite(m)
        //else remove m
      case _ => rewriteExpression(m)
    }
  }

  override def visit(n : NameExpression) : Unit = rewriteExpression(n)

  override def visit(d : Dereference) : Unit = rewriteExpression(d)

  private def rewriteExpression(e : ASTNode) : Unit =
    if(isSingleRoleNameExpression(e,roleNames))
      result = copy_rw.rewrite(e)
    else result = create.constant(true)

  private def selectResourceAnnotation(n :ASTNode) : ASTNode =
    n match {
      case e : OperatorExpression => e.operator match {
        case StandardOperator.Perm => n
        case StandardOperator.NEQ => if (isNullNode(e.first) || isNullNode(e.second)) n else create.constant(true)
        case StandardOperator.Star => create.expression(e.operator,selectResourceAnnotation(e.first), selectResourceAnnotation(e.second))
        case _ => create.constant(true)
      }
      case mi : MethodInvokation => mi.getType match {
        case p : PrimitiveType => if(p.sort == PrimitiveSort.Resource) n else create.constant(true)
        case _ => create.constant(true)
      }
      case _ => create.constant(true)
    }

  private def isNullNode(n : ASTNode) : Boolean =
    n match {
      case name : NameExpression => name.reserved == ASTReserved.Null
      case _ => false
    }

  private def getChanVar(role : NameExpression, isWrite : Boolean) =  create.field_name(getChanName(if(isWrite) (roleName + role.name) else (role.name + roleName)))

  def isSingleRoleNameExpression(e : ASTNode, roleNames : Iterable[String]) : Boolean = {
    val expRoles = getNamesFromExpression(e).filter(n => roleNames.contains(n.name))
    expRoles.isEmpty || (expRoles.size == 1 && expRoles.head.name == roleName)
  }

}