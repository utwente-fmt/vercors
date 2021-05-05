package vct.col.veymont

import hre.ast.MessageOrigin
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelBlock}
import vct.col.ast.stmt.decl._
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.veymont.Util._

import scala.collection.convert.ImplicitConversions.{`collection asJava`, `iterable AsScalaIterable`}

class Decompose(override val source: ProgramUnit) extends AbstractRewriter(null, true) {

  private val roleNames : Iterable[String] = StructureCheck.getRoleNames(source)
  private val mainClass = StructureCheck.getMainClass(source)
  private val roleOrOtherClass = StructureCheck.getRoleOrHelperClass(source)
  private var roleName : String = null

  private var chans : Set[ChannelRepr] = Set()
  private var cloneClasses : Set[ASTDeclaration] = Set()

  def addThreadClasses() : ProgramUnit = {
    roleNames.foreach(role => {
      target().add(createThreadClass(role))
    })
    roleName = null
    source.get().filter(_.name != mainClassName).foreach(c =>
      if(isChannelClass(c.name)) {// && cloneClasses.nonEmpty) //annotations for readValue?
    /*    val chanSorts = chans.map(_.chanType match {
          case p : PrimitiveType => p.sort
          case ct : ClassType => ct.getType
        }) */
        if(chans.exists(_.chanType.toString == getTypeChannelClass(c.name))) //only add used channel classes
          target().add(c)
      }
      else if(c.name == barrierClassName)
        target.add(c)
      else if(cloneClasses.exists(_.name == c.name))
        target().add(rewrite(addClone(c.asInstanceOf[ASTClass])))
      else
        target().add(rewrite(c)))
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
    chans.foreach(chan => thread.add_dynamic(create.field_decl(chan.channel,chan.getChanClass())))
    thread.add_dynamic(create.field_decl(barrierFieldName,getBarrierClass()))
    rewMethods.foreach(thread.add)
    create.leave()
    thread
  }

  private def addClone(c : ASTClass) : ASTClass = {
    c.methods().find(_.name == cloneMethod) match {
      case Some(_) => Fail("VeyMont Fail: Class %s is not allowed to have a method with name '%s'",c.name,cloneMethod); c
      case None => {
        create.enter()
        create.setOrigin(new MessageOrigin("Generated clone method in class " + c.name))
        val contract = new ContractBuilder()
        c.fields().forEach(f => contract.context(create.expression(StandardOperator.Perm,create.field_name(f.name),create.reserved_name(ASTReserved.ReadPerm))))
        c.fields().forEach(f => contract.ensures(create.expression(StandardOperator.Perm,create.dereference(create.reserved_name(ASTReserved.Result),f.name),create.constant(1))))
        val m = create.method_decl(create.class_type(c.name,Array[DeclarationStatement]():_*),contract.getContract,cloneMethod,Array[DeclarationStatement]() ,null)
        c.add_dynamic(m)
        create.leave()
        c
      }
    }
  }

  override def visit(m : Method) : Unit = { //assume ony pre and postconditions
    val c = m.getContract()
    val cb = new ContractBuilder()
    cb.requires(rewrite(selectResourceAnnotation(c.pre_condition)))
    cb.ensures(rewrite(selectResourceAnnotation(c.post_condition)))
    if(m.kind == Method.Kind.Constructor && roleName != null) {
      result = create.method_kind(m.kind, m.getReturnType, cb.getContract, getThreadClassName(roleName), m.getArgs, rewrite(m.getBody))
      //  } else if(m.kind == Method.Kind.Pure) {
      //    result = copy_rw.rewrite(m)
    } else if(m.kind == Method.Kind.Predicate) {
      result = create.method_kind(m.kind,m.getReturnType,cb.getContract,m.name,m.getArgs,rewrite(selectResourceAnnotation(m.getBody)))
    } else {
      result = create.method_kind(m.kind,m.getReturnType,cb.getContract,m.name,m.getArgs,rewrite(m.getBody))
    }
  }

  override def visit(l : LoopStatement) : Unit = { //it is while loop
    val c = l.getContract
    val cb = new ContractBuilder()
    cb.appendInvariant(rewrite(selectResourceAnnotation(c.invariant)))
    result = create.while_loop(rewrite(l.getEntryGuard),rewrite(l.getBody),cb.getContract)
  }

  override def visit(pb : ParallelBlock) : Unit = {
    val c = pb.contract
    val cb = new ContractBuilder()
    cb.requires(rewrite(selectResourceAnnotation(c.pre_condition)))
    cb.ensures(rewrite(selectResourceAnnotation(c.post_condition)))
    result = create.parallel_block(pb.label,cb.getContract,pb.itersJava,rewrite(pb.block),pb.deps)
  }

  override def visit(a : AssignmentStatement) : Unit = {
    if(roleName == null) {
      super.visit(a)
    } else {
      getLocalAction(a, roleName) match {
        case SingleRoleAction(_) => result = copy_rw.rewrite(a)
        case ReadAction(receiver, sender, receiveExpression) => {
          val chanType = receiveExpression.getType
          val chanName = getChanName(sender, false, chanType)
          chans += new ChannelRepr(chanName, false, chanType)
          result = create.assignment(receiveExpression, create.invokation(create.field_name(chanName), null, chanRead))
        }
        case WriteAction(receiver, _, sendExpression) => {
          val chanType = sendExpression.getType
          val chanName = getChanName(receiver, true, chanType)
          chans += new ChannelRepr(chanName, true, chanType)
          if (chanType.isNumeric || chanType.isBoolean)
            result = create.invokation(create.field_name(chanName), null, chanWrite, sendExpression)
          else {
            roleOrOtherClass.find(c => c.name == chanType.toString) match {
              case Some(c) => {
                cloneClasses = cloneClasses + c
                result = create.invokation(create.field_name(chanName), null, chanWrite, create.invokation(sendExpression, null, "clone"))
              }
              case None =>
                Fail("VeyMont Fail: channel of type %s not supported", chanType)
            }
          }
        }
        case Tau => result = create.special(ASTSpecial.Kind.TauAction, Array[ASTNode](): _*)
        case _ => Fail("VeyMont Fail: assignment %s is no session assignment! ", a.toString)
      }
    }
  }

  def getLocalAction(a: AssignmentStatement, roleName : String) : LocalAction = {
    val expRole = getNamesFromExpression(a.expression)
    getNameFromNode(a.location) match {
      case Some(locRole) => {
        if(locRole.name == roleName && (expRole.isEmpty || expRole.size == 1 && expRole.head.name == roleName))
          SingleRoleAction(a)
        else if(locRole.name == roleName && expRole.size == 1 && expRole.head.name != roleName)
          ReadAction(locRole,expRole.head,a.location)
        else if(locRole.name != roleName && expRole.size == 1 && expRole.head.name == roleName)
          WriteAction(locRole,roleName,a.expression)
        else if(locRole.name != roleName && (expRole.isEmpty || expRole.size == 1 && expRole.head.name != roleName))
          Tau
        else ErrorAction
      }
      case None => ErrorAction
    }
  }

  override def visit(e : OperatorExpression) : Unit = {
    if(roleName == null) {
      super.visit(e)
    } else {
      if (e.operator == StandardOperator.Star || e.operator == StandardOperator.And) {
        result = create.expression(e.operator, rewrite(e.first), rewrite(e.second))
      } else rewriteExpression(e)
    }
  }

  override def visit(m : MethodInvokation) : Unit = {
    if(roleName == null) {
      super.visit(m)
    } else {
      m.getParent match {
        case b: BlockStatement => //it is a statement
          if (isSingleRoleNameExpression(m, roleNames))
            result = copy_rw.rewrite(m)
          else result = create.special(ASTSpecial.Kind.TauAction, Array[ASTNode](): _*)
        case _ => rewriteExpression(m)
      }
    }
  }

  override def visit(n : NameExpression) : Unit = if(roleName == null) super.visit(n) else rewriteExpression(n)

  override def visit(d : Dereference) : Unit = if(roleName == null) super.visit(d) else rewriteExpression(d)

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

  private def getChanName(role : NameExpression, isWrite : Boolean, chanType : Type) : String =
    (if(isWrite) (roleName + role.name) else (role.name + roleName)) + chanType.toString + chanName

  def isSingleRoleNameExpression(e : ASTNode, roleNames : Iterable[String]) : Boolean = {
    val expRoles = getNamesFromExpression(e).filter(n => roleNames.contains(n.name))
    expRoles.isEmpty || (expRoles.size == 1 && expRoles.head.name == roleName)
  }

}