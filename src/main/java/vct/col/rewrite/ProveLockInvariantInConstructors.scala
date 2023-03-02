package vct.col.rewrite

import hre.ast.BranchOrigin
import vct.col.ast.`type`.Type
import vct.col.ast.stmt.decl.{ASTClass, ASTSpecial, Method, ProgramUnit}
import vct.col.ast.util.AbstractRewriter

import scala.jdk.CollectionConverters._

case class ProveLockInvariantInConstructors(override val source: ProgramUnit) extends AbstractRewriter(source) {
  private var haveLockInvariant = false

  override def visit(cls: ASTClass): Unit = {
    val lockInvariants: Seq[Method] = cls.asScala.toSeq.filter {
      case method: Method =>
        method.kind == Method.Kind.Predicate && method.name == "lock_invariant"
      case _ => false
    }.map(_.asInstanceOf[Method])

    if(lockInvariants.size >= 2) {
      lockInvariants(1).getOrigin.report("error", "A class may have only one lock invariant")
      throw new Error()
    }

    if(lockInvariants.size == 1 && lockInvariants.head.getArgs.nonEmpty) {
      lockInvariants.head.getOrigin.report("error", "The lock invariant predicate may not have arguments (other than 'this')")
      throw new Error()
    }

    val constructors = cls.asScala.toSeq.filter {
      case method: Method => method.kind == Method.Kind.Constructor
      case _ => false
    }

    if (constructors.isEmpty && lockInvariants.nonEmpty) {
      lockInvariants.head.getOrigin.report("error", "cannot generate implicit constructor for class with lock invariant")
      throw new Error()
    }

    haveLockInvariant = lockInvariants.nonEmpty

    super.visit(cls)
  }

  override def visit(method: Method): Unit = {
    if(method.kind == Method.Kind.Constructor && haveLockInvariant) {
      val body = create.block()
      body.addStatement(method.getBody)

      create.enter()
      create.setOrigin(new BranchOrigin("Proof of the lock invariant in the constructor", method.getOrigin))

      body.addStatement(create special(ASTSpecial.Kind.Fold, create.invokation(create.diz(), null, "lock_invariant")))
      body.addStatement(create special(ASTSpecial.Kind.Exhale, create.invokation(create.diz(), null, "lock_invariant")))

      create.leave()

      result = create method_kind(
        Method.Kind.Constructor,
        rewrite(method.getReturnType),
        rewrite[Type](method.signals),
        rewrite(method.getContract),
        method.name,
        rewrite(method.getArgs),
        method.usesVarArgs(),
        body
      )
    } else {
      super.visit(method)
    }
  }
}
