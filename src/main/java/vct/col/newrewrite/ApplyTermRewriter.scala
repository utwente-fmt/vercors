package vct.col.newrewrite

import hre.util.FuncTools
import vct.col.ast._
import vct.col.newrewrite.util.Substitute
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter, RewriterBuilder}
import vct.result.VerificationResult.{Unreachable, UserError}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

case object ApplyTermRewriter {
  case class BuilderFor[Rule](ruleNodes: Seq[SimplificationRule[Rule]]) extends RewriterBuilder {
    override def apply[Pre <: Generation](): Rewriter[Pre] = ApplyTermRewriter(ruleNodes)
  }
}

case class ApplyTermRewriter[Rule, Pre <: Generation](ruleNodes: Seq[SimplificationRule[Rule]]) extends Rewriter[Pre] {
  case class MalformedSimplificationRule(body: Expr[_]) extends UserError {
    override def code: String = "malformedSimpRule"
    override def text: String =
      body.o.messageInContext(
        "The body of a simplification rule must be any number of nested \\forall predicates, " +
          "of which the body is an equality.")
  }

  val rules: Seq[(Seq[Variable[Rule]], Expr[Rule], Expr[Rule])] = ruleNodes.map(node => consumeForalls(node.axiom)).map {
    case (free, body) => body match {
      case Eq(left, right) => (free, left, right)
      case other => throw MalformedSimplificationRule(other)
    }
  }

  def consumeForalls(node: Expr[Rule]): (Seq[Variable[Rule]], Expr[Rule]) = node match {
    case Forall(bindings, _, body) =>
      val (innerBindings, innerBody) = consumeForalls(body)
      (bindings ++ innerBindings, innerBody)
    case other => (Nil, other)
  }

  case class CopyRule() extends NonLatchingRewriter[Rule, Pre] {
    override def succ[DPost <: Declaration[Pre]](decl: Declaration[Rule])(implicit tag: ClassTag[DPost]): Ref[Pre, DPost] =
      new LazyRef[Pre, DPost](successionMap.get(decl).getOrElse(decl.asInstanceOf[Declaration[Pre]]))
  }

  def apply(rule: (Seq[Variable[Rule]], Expr[Rule], Expr[Rule]), subject: Expr[Pre]): Option[Expr[Pre]] = {
    val (free, pattern, subtitute) = rule
    val inst = mutable.Map[Expr[Rule], Expr[Pre]]()
    val typeInst = mutable.Map[TVar[Rule], Type[Pre]]()

    val debugNoMatch = true
    val debugMatch = true

    implicit val o: Origin = DiagnosticOrigin

    Comparator.compare(pattern, subject).foreach {
      case Comparator.MatchingDeclaration(left: Variable[Rule], right: Variable[Pre]) =>
        if(!left.t.superTypeOf(right.t.asInstanceOf[Type[Rule]])) {
          if(debugNoMatch)
            println(s"Expression `$subject` does not match rule `$pattern`, since ${right.t} (the type of $right) is not a subtype of ${left.t} (the type of $left)")
          return None
        }

        left.t match {
          case TType(_) =>
            if(typeInst.getOrElseUpdate(TVar(left.ref), TVar(right.ref)) != TVar(right.ref[Variable[Pre]])) {
              if(debugNoMatch)
                println(s"Expression `$subject` does not match rule `$pattern`, since earlier $left matched to the type `${typeInst(TVar(left.ref))}`, but now it must match to $right")
              return None
            }
          case _ =>
            if(inst.getOrElseUpdate(Local(left.ref), Local(right.ref)) != Local(right.ref[Variable[Pre]])) {
              if(debugNoMatch)
                println(s"Expression `$subject` does not match rule `$pattern`, since earlier $left matched to `${inst(Local(left.ref))}`, but now it must match to $right")
              return None
            }
        }
      case Comparator.MatchingDeclaration(_, _) =>
        throw Unreachable("Wrong declaration kind for simplification rule.")
      case Comparator.MatchingReference(left: Variable[Rule], right: Variable[Pre]) =>
        if(!left.t.superTypeOf(right.t.asInstanceOf[Type[Rule]])) {
          if(debugNoMatch)
            println(s"Expression `$subject` does not match rule `$pattern`, since ${right.t} (the type of $right) is not a subtype of ${left.t} (the type of $left)")
          return None
        }

        left.t match {
          case TType(_) =>
            if(typeInst.getOrElseUpdate(TVar(left.ref), TVar(right.ref)) != TVar(right.ref[Variable[Pre]])) {
              if(debugNoMatch)
                println(s"Expression `$subject` does not match rule `$pattern`, since earlier $left matched to the type `${typeInst(TVar(left.ref))}`, but now it must match to $right")
              return None
            }
          case _ =>
            if(inst.getOrElseUpdate(Local(left.ref), Local(right.ref)) != Local(right.ref[Variable[Pre]])) {
              if(debugNoMatch)
                println(s"Expression `$subject` does not match rule `$pattern`, since earlier $left matched to `${inst(Local(left.ref))}`, but now it must match to $right")
              return None
            }
        }
      case Comparator.MatchingReference(_, _) =>
        throw Unreachable("Wrong declaration kind for simplification rule.")
      case Comparator.StructuralDifference(left, right) =>
        if(debugNoMatch)
          println(s"Expression `$subject` does not match rule `$pattern`, since `$right` does not match `$left`")
        return None
    }

    // Free declarations are transmuted from Declaration[Rule] -> Declaration[Pre] when copying, so we can safely transmute
    // the inst keys. It would be a bit cleaner to make fresh [Pre] copies of left and right to avoid these issues,
    // but that is much less performant.

    val preInst = inst.map { case (k, v) => k.asInstanceOf[Expr[Pre]] -> v }.toMap
    val preTypeInst = typeInst.map { case (k, v) => k.asInstanceOf[TVar[Pre]] -> v }.toMap
    val result = Substitute(preInst, preTypeInst).dispatch(CopyRule().dispatch(subtitute))

    if(debugMatch) {
      println(s"Expression:       $subject")
      println(s"Matches:          $pattern")
      if(inst.nonEmpty) {
        println("With bindings:")
        inst.foreach {
          case (rule, binding) => println(s"  $rule = $binding")
        }
      }
      if(typeInst.nonEmpty) {
        println("With type bindings:")
        typeInst.foreach {
          case (rule, binding) => println(s"  $rule = $binding")
        }
      }
      println(s"Applied to:       $subtitute")
      println(s"Result:           $result")
      println()
    }

    Some(result)
  }

  @tailrec
  final def applyExhaustively(expr: Expr[Pre]): Expr[Pre] =
    FuncTools.firstOption(rules, apply(_, expr)) match {
      case Some(e) =>
        applyExhaustively(e)
      case None => expr
    }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    rewriteDefault(applyExhaustively(e))
}
