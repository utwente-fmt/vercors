package vct.col.veymont

import hre.ast.MessageOrigin
import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.expr._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelBlock}
import vct.col.ast.stmt.decl._
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.veymont.GenerateTypedChannel.getTypeName
import vct.col.veymont.StructureCheck.{getRoleObjects, isAllowedPrimitive, isMapType, isOptionOfArrayType, isSequenceType}
import vct.col.veymont.Util._

import scala.jdk.CollectionConverters._

class Decompose(override val source: ProgramUnit) extends AbstractRewriter(source) {

  private val roleNames : Iterable[String] = StructureCheck.getRoleNames(source)
  private val mainClass = StructureCheck.getMainClass(source)
  private val roleOrOtherClass = StructureCheck.getRoleOrHelperClass(source)
  private val roleClassNames : Iterable[String] = StructureCheck.getRoleClassNames(source)
  private var roleName : Option[String] = None

  private var chans = Set.empty[ChannelRepr]
  private var allChans = Set.empty[ChannelRepr]

  def addThreadClasses() : ProgramUnit = {
    roleNames.foreach(role => {
      target().add(createThreadClass(role))
    })
    roleName = None
    source.get().asScala.foreach{sourceEl =>
      val c = sourceEl match {
        case ns : NameSpace => ns.get(0)
        case other => other
      }
      if(c.name == mainClassName)
        target().add(c)
      else if(isChannelClass(c.name)) {
        val chanTypes = allChans.map(ch => ch.chanType match {
          case p : PrimitiveType => Left(p)
          case ct : ClassType => roleOrOtherClass.find(_.name == ct.getName) match {
            case Some(roleCl) => Right(roleCl)
            case None => throw Failure("VeyMont Fail: Channel of type %s not supported!",ct.getName)
          }
          case o => throw Failure("VeyMont Fail: Unexpected channel type: %s",o)
        })
        val chanClassProg = new ProgramUnit()
        chanClassProg.add(sourceEl)
        val newChansClasses = chanTypes.map(t => new GenerateTypedChannel(chanClassProg,t).rewriteAll().get(0))
        newChansClasses.foreach(target().add(_))
      }
      else
        target().add(rewrite(c))}
    target()
  }

  private def createThreadClass(role : String) = {
    create.enter()
    roleName = Some(role)
    chans = Set.empty
    create.setOrigin(new MessageOrigin("Generated thread class " + roleName.get))
    val threadName = getThreadClassName(roleName.get)
    val thread = create.new_class(threadName,null,null)
    val rewMethods = mainClass.methods().asScala.filter(_.name != mainMethodName).map(rewrite(_))
    mainClass.fields().forEach(f => if(f.name == roleName.get) thread.add(rewrite(f)))
    chans.foreach(chan => thread.add_dynamic(create.field_decl(chan.channel,chan.getChanClass)))
    rewMethods.foreach(thread.add)
    create.leave()
    thread
  }

  override def visit(m : Method) : Unit = { //assume ony pre and postconditions
    val cb = new ContractBuilder()
    val c = m.getContract()
    if(c != null) {
      val pre = selectResourceAnnotation(c.pre_condition)
      val post = selectResourceAnnotation(c.post_condition)
      checkAnnotation(pre, roleNames)
      checkAnnotation(post, roleNames)
      cb.requires(rewrite(pre))
      cb.ensures(rewrite(post))
    }
    if(m.getParent match {
      case c : ASTClass => c.name == channelClassName
      case _ => false
    })
      result = copy_rw.rewrite(m)
    else if(m.kind == Method.Kind.Constructor && roleName.nonEmpty) {
      result = create.method_kind(m.kind, m.getReturnType, cb.getContract(c == null), getThreadClassName(roleName.get), filterRoleArgs(m), rewrite(m.getBody))
    } else if(m.kind == Method.Kind.Predicate) {
      val body = selectResourceAnnotation(m.getBody)
      checkAnnotation(body,roleNames)
      result = create.method_kind(m.kind,m.getReturnType,cb.getContract(c == null),m.name,m.getArgs,rewrite(body))
    } else {
      val cbc = cb.getContract(c == null)
      val newMethod = create.method_kind(m.kind,m.getReturnType,cbc,m.name,m.getArgs,rewrite(m.getBody))
      result = newMethod
    }
  }

  private def filterRoleArgs(constr : Method) : Array[DeclarationStatement] = {
    val roleAssign = getRoleObjects(constr).find(_.location.asInstanceOf[NameExpression].name == roleName.get).get
    val roleArgs = roleAssign.expression.asInstanceOf[MethodInvokation].args.flatMap(getNamesFromExpression).toSet
    constr.getArgs.filter(arg => roleArgs.exists(_ match {
      case a : NameExpression => arg.name == a.name
      case _ => false
    }))
  }

  override def visit(l : LoopStatement) : Unit = {
    val c = l.getContract
    val cb = new ContractBuilder()
    val invar = selectResourceAnnotation(c.invariant)
    checkAnnotation(invar, roleNames)
    cb.appendInvariant(rewrite(invar))
    if(l.getInitBlock != null || l.getUpdateBlock != null) //could also use the Flatten pass before this pass such that we only have while loops here
      result = create.for_loop(rewrite(l.getInitBlock), rewrite(l.getEntryGuard),rewrite(l.getUpdateBlock),rewrite(l.getBody),cb.getContract(c == null))
    else result = create.while_loop(rewrite(l.getEntryGuard),rewrite(l.getBody),cb.getContract(c == null))
  }

  override def visit(pb : ParallelBlock) : Unit = {
    val c = pb.contract
    val cb = new ContractBuilder()
    val pre = selectResourceAnnotation(c.pre_condition)
    val post = selectResourceAnnotation(c.post_condition)
    checkAnnotation(pre, roleNames)
    checkAnnotation(post,roleNames)
    cb.requires(rewrite(pre))
    cb.ensures(rewrite(post))
    result = create.parallel_block(pb.label,cb.getContract,pb.itersJava,rewrite(pb.block),pb.deps)
  }

  override def visit(a : AssignmentStatement) : Unit = {
    if(roleName.isEmpty) {
      super.visit(a)
    } else {
      getLocalAction(a, roleName.get,roleNames) match {
        case SingleRoleAction(_) => result = copy_rw.rewrite(a)
        case ReadAction(_, sender, receiveExpression) => {
          val chanType = receiveExpression.getType
          val chanName = getChanName(sender, false, chanType)
          val chanRepr = ChannelRepr(chanName)(false, chanType)
          chans += chanRepr
          allChans += chanRepr
          result = create.assignment(receiveExpression, create.invokation(create.field_name(chanName), null, chanReadMethodName))
        }
        case WriteAction(receiver, _, sendExpression) => {
          val chanType = sendExpression.getType
          val writeChanName = getChanName(receiver, true, chanType)
          val chanRepr = ChannelRepr(writeChanName)(true, chanType)
          chans += chanRepr
          allChans += chanRepr
          chanType match {
            case p : PrimitiveType => checkChanPrimitiveType(p,writeChanName,sendExpression,a)
            case cl : ClassType => checkChanClassClone(sendExpression,writeChanName,chanType)
          }
        }
        case Tau => //result = create.special(ASTSpecial.Kind.TauAction, Array.empty[ASTNode]: _*)
        case _ => Fail("VeyMont Fail: assignment %s is no VeyMont global program assignment! ", a.toString)
      }
    }
  }

  private def checkChanPrimitiveType(p : PrimitiveType, writeChanName : String, sendExpression : ASTNode, a : AssignmentStatement) =
    if(isAllowedPrimitive(p)) {
      sendExpression match {
      //  case op : OperatorExpression => if(op.operator == StandardOperator.Subscript) Fail("VeyMont Fail: channels for array elements not supported in: %s!",a)
        case _ => //skip
      }
      result = create.invokation(create.field_name(writeChanName), null, chanWriteMethodName, sendExpression)
    } else if(isOptionOfArrayType(p,false,List.empty)) {
      result = create.invokation(create.field_name(writeChanName), null, chanWriteMethodName, sendExpression)
      //  result = getCloneWriteInvocation(writeChanName,sendExpression)
    } else if(isSequenceType(p, false, List.empty) || isMapType(p, false, List.empty)) {
      result = create.invokation(create.field_name(writeChanName), null, chanWriteMethodName, sendExpression)
      //  result = getCloneWriteInvocation(writeChanName,sendExpression)
    } else Fail("VeyMont Fail: channel of type %s not supported", p)

  private def checkChanClassClone(sendExpression : ASTNode, writeChanName : String, chanType : Type) : Unit = {
    sendExpression match {
      case m : MethodInvokation =>
        if(m.method == Util.cloneMethod)
          result = create.invokation(create.field_name(writeChanName), null, chanWriteMethodName, sendExpression)
        else Fail("VeyMont Fail: send object %s must be cloned for sending", m.`object`.toString)
      case _ => if(chanType.toString != Util.stringType) Fail("VeyMont Fail: send object %s must be cloned for sending", sendExpression.toString)
    }
  }

  def getLocalAction(a: AssignmentStatement, roleName : String, roleNames : Iterable[String]) : LocalAction = {
    val expRole = getNamesFromExpression(a.expression).filter(n => roleNames.exists(_ == n.name))
    getNameFromNode(a.location) match {
      case Some(locRole) => {
        if(locRole.name == roleName && (expRole.isEmpty || expRole.size == 1 && expRole.head.name == roleName || isRoleConstructorCall(a.expression)))
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

  private def isRoleConstructorCall(exp : ASTNode) : Boolean = exp match {
    case m : MethodInvokation => m.method == Method.JavaConstructor && roleClassNames.exists(_ == m.dispatch.getName)
    case _ => false
  }

  override def visit(e : OperatorExpression) : Unit = {
    if(roleName.isEmpty) {
      super.visit(e)
    } else {
      if (e.operator == StandardOperator.Star || e.operator == StandardOperator.And) {
        result = create.expression(e.operator, rewrite(e.first), rewrite(e.second))
      } else rewriteExpression(e)
    }
  }

  override def visit(m : MethodInvokation) : Unit = {
    if(roleName.isEmpty) {
      super.visit(m)
    } else {
      m.getParent match {
        case _: BlockStatement => //it is a statement
          if (isSingleRoleNameExpressionOfRole(m, roleNames))
            result = copy_rw.rewrite(m)
          //else result = create.special(ASTSpecial.Kind.TauAction, Array.empty[ASTNode]: _*)
        case _ => rewriteExpression(m)
      }
    }
  }

  override def visit(a : ASTSpecial) : Unit = {
    if(a.kind != ASTSpecial.Kind.Assert) { //don't project assertions
      super.visit(a)
    }
  }

  override def visit(n : NameExpression) : Unit = if(roleName.isEmpty) super.visit(n) else rewriteExpression(n)

  override def visit(d : Dereference) : Unit = if(roleName.isEmpty) super.visit(d) else rewriteExpression(d)

  private def rewriteExpression(e : ASTNode) : Unit =
    if(isSingleRoleNameExpressionOfRole(e,roleNames))
      result = copy_rw.rewrite(e)
    else result = create.constant(true)

  private def selectResourceAnnotation(n :ASTNode) : ASTNode = create.constant(true)
    /* n match {
      case e : OperatorExpression => e.operator match {
        case StandardOperator.Perm => n
        case StandardOperator.NEQ => if (isNullNode(e.first) || isNullNode(e.second)) n else create.constant(true)
        case StandardOperator.Implies =>
          if(selectResourceAnnotation(e.first) match {
          case eop : OperatorExpression => isNullNode(eop.first) || isNullNode(eop.second)
          case _ => false
              })
            create.expression(e.operator,selectResourceAnnotation(e.first), selectResourceAnnotation(e.second))
          else create.constant(true)
        case StandardOperator.Star => create.expression(e.operator,selectResourceAnnotation(e.first), selectResourceAnnotation(e.second))
        case _ => create.constant(true)
      }
      case mi : MethodInvokation => mi.getType match {
        case p : PrimitiveType => if(p.sort == PrimitiveSort.Resource) n else create.constant(true)
        case _ => create.constant(true)
      }
      case _ => create.constant(true)
    } */

  private def isNullNode(n : ASTNode) : Boolean =
    n match {
      case name : NameExpression => name.reserved == ASTReserved.Null
      case _ => false
    }

  private def getChanName(role : NameExpression, isWrite : Boolean, chanType : Type) : String =
    (if(isWrite) (roleName.get + role.name) else (role.name + roleName.get)) + getTypeName(chanType) + chanName

  def isSingleRoleNameExpressionOfRole(e : ASTNode, roleNames : Iterable[String]) : Boolean = {
    val expRoles = getNamesFromExpression(e).filter(n => roleNames.exists(_ == n.name))
    expRoles.isEmpty || (expRoles.size == 1 && expRoles.head.name == roleName.get)
  }

  def isSingleRoleNameExpression(e : ASTNode, roleNames : Iterable[String]) : Boolean = {
    val expRoles = getNamesFromExpression(e).filter(n => roleNames.exists(_ == n.name))
    expRoles.isEmpty || expRoles.size == 1
  }

  private def checkAnnotation(n : ASTNode, roleNames : Iterable[String]) : Unit = {
    val wrong = splitOnStar(n).filter(op => !isSingleRoleNameExpression(op,roleNames))
    wrong.foreach(op => Fail("VeyMont Fail: annotation refers to multiple roles: %s!",op))
  }

  private def splitOnStar(node : ASTNode) : List[ASTNode] = {
    node match {
      case e : OperatorExpression => e.operator match {
        case StandardOperator.Star => splitOnStar(e.first) ++ splitOnStar(e.second)
        case _ => List(node)
      }
      case _ => List(node)
    }
  }

}