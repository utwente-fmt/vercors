package vct.col.veymont

import hre.ast.MessageOrigin
import vct.col.ast.`type`.ASTReserved
import vct.col.ast.expr.StandardOperator
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.{ASTClass, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, ContractBuilder}
import vct.col.veymont.Util.{isChannelClass, isThreadClassName}

import scala.collection.convert.ImplicitConversions.`iterable AsScalaIterable`

/**
  * Adds the channels to Thread constructors
  * @param source
  */
class LocalProgConstructors(override val source: ProgramUnit)  extends AbstractRewriter(null, true) {

  var chanMap = Map.empty[String,Set[ChannelRepr]]

  def addChansToConstructors : ProgramUnit = {
    for(entry <- source.get()) {
      entry match {
        case c : ASTClass => {
          if(isThreadClassName(c.name)) {
            val chans = getChansFromFields(c)
            chanMap += (c.name -> chans)
          }
        }
      }
    }
    rewriteAll
  }

  private def getChansFromFields(c : ASTClass) : Set[ChannelRepr] = {
    val role = c.fields().head.name //role is first field
    c.fields().filter(f => isChannelClass(f.`type`.toString)).map(f => new ChannelRepr(f.name,f.name.startsWith(role),f.`type`)).toSet
  }

  override def visit(m : Method) = {
    if(m.kind == Method.Kind.Constructor && isThreadClassName(m.name)) {
      create.enter()
      create.setOrigin(new MessageOrigin("Generated constructor " + m.name))
      val chans = chanMap.get(m.name).get
      val chanArgs : Set[DeclarationStatement] = chans.map(chan => create.field_decl(chan.getArgChanName(),chan.chanType))
      val newContract = getRoleConstructorContract(chans)
      rewrite(m.getContract,newContract)
      val chanDecls : Array[ASTNode] = chans.map(chan =>
        create.assignment(create.field_name(chan.channel),
                          create.argument_name(chan.getArgChanName()))).toArray
      val threadDecl : Array[ASTNode] = m.getBody match {
          case b : BlockStatement => b.getStatements
        case _ => throw Failure("Constructor %s must have a BlockStatement body",m.name)
      }
      val allStats = rewrite(threadDecl) ++ chanDecls
      val args : Array[DeclarationStatement] = rewrite(m.getArgs) ++ chanArgs.toArray[DeclarationStatement]
      val body : BlockStatement = create.block(allStats:_*)
      val myNewMethod = create.method_kind(m.kind,m.getReturnType,newContract.getContract,m.name,args,body)
      create.leave()
      result = myNewMethod
    } else {
      super.visit(m)
    }
  }

  private def getRoleConstructorContract(chans : Set[ChannelRepr]) = {
    val contract = new ContractBuilder()
    val reqSentRecvd = chans.map(_.getArgChan().getChanFieldPerm(create))
    val reqNotNull = chans.map(chan =>
      create.expression(StandardOperator.NEQ,
        create.argument_name(chan.getArgChanName()),
        create.reserved_name(ASTReserved.Null)))
    val ensPerm = chans.map(chan =>
      create.expression(StandardOperator.Perm,
        create.field_name(chan.channel),
        create.reserved_name(ASTReserved.ReadPerm)))
    val ensArgEq = chans.map(chan =>
      create.expression(StandardOperator.EQ,
        create.field_name(chan.channel),
        create.argument_name(chan.getArgChanName())))
    val ensSentRecvd = chans.map(_.getChanFieldPerm(create))
  //  reqSentRecvd.foreach(contract.requires(_))
    reqNotNull.foreach(contract.requires(_))
    ensPerm.foreach(contract.ensures(_))
    ensArgEq.foreach(contract.ensures(_))
  //  ensSentRecvd.foreach(contract.ensures(_))
    contract
  }



}
