/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.silver.ast.utility

import viper.silver.ast._

/**
 * An implementation for transformers of the SIL AST.
 */
object Transformer {

  /** Transforms the tree rooted at this node using the partial function `pre`,
    * recursing on the subnodes and finally using the partial function `post`.
    *
    * The previous node is replaced by applying `pre` and `post`, respectively,
    * if and only if these partial functions are defined there. The functions
    * `pre` and `post` must produce nodes that are valid in the given context.
    * For instance, they cannot replace an integer literal by a Boolean literal.
    *
    * @param pre       Partial function used before the recursion.
    *                  Default: partial function with the empty domain.
    * @param recursive Given the original node, should the children of the node
    *                  transformed with `pre` be transformed recursively? `pre`,
    *                  `recursive` and `post` are kept the same during each
    *                  recursion.
    *                  Default: recurse if and only if `pre` is not defined
    *                  there.
    * @param post      Partial function used after the recursion.
    *                  Default: partial function with the empty domain.
    *
    * @return Transformed tree.
    */
  def transform[A <: Node]
               (node: A, pre: PartialFunction[Node, Node] = PartialFunction.empty)
               (recursive: Node => Boolean = !pre.isDefinedAt(_),
                post: PartialFunction[Node, Node] = PartialFunction.empty)
               : A = {

    def go[B <: Node](root: B): B = {
      transform(root, pre)(recursive, post)
    }

    def goTypeVariables(mapping: Map[TypeVar, Type]): Map[TypeVar, Type] = {
      mapping.toSeq.map(pair => go(pair._1) -> go(pair._2)).toMap
    }

    def recurse(parent: Node): Node = {
      parent match {
        case exp: Exp =>
          val p = exp.pos
          val i = exp.info
          exp match {
            case IntLit(_) => exp
            case BoolLit(_) => exp
            case NullLit() => exp
            case AbstractLocalVar(_) => exp
            // AS: added recursion on field: this was previously missing (as for all "shared" nodes in AST). But this could lead to the type of the field not being transformed consistently with its declaration (if the whole program is transformed)
            case FieldAccess(rcv, field) => FieldAccess(go(rcv), go(field))(p, i)
            case PredicateAccess(params, predicateName) =>
              PredicateAccess(params map go, predicateName)(p, i)

            case Unfolding(acc, e) => Unfolding(go(acc), go(e))(p, i)

            case UnfoldingGhostOp(acc, e) => UnfoldingGhostOp(go(acc), go(e))(p, i)
            case FoldingGhostOp(acc, e) => FoldingGhostOp(go(acc), go(e))(p, i)
            case ApplyingGhostOp(wand, in) => ApplyingGhostOp(go(wand), go(in))(p, i)
            case PackagingGhostOp(wand, in) => PackagingGhostOp(go(wand), go(in))(p, i)

            case Old(e) => Old(go(e))(p, i)
            case LabelledOld(e,lbl) => LabelledOld(go(e),lbl)(p,i)
            case ApplyOld(e) => ApplyOld(go(e))(p, i)
            case CondExp(cond, thn, els) =>
              CondExp(go(cond), go(thn), go(els))(p, i)
            case Let(v, exp1, body) => Let(go(v), go(exp1), go(body))(p, i)
            case Exists(v, e) => Exists(v map go, go(e))(p, i)
            case Forall(v, triggers, e) =>
              Forall(v map go, triggers map go, go(e))(p, i)
            case ForPerm(v, fields, e) =>
              ForPerm(go(v), fields map go, go(e))(p,i)
            case InhaleExhaleExp(in, ex) =>
              InhaleExhaleExp(go(in), go(ex))(p, i)
            case WildcardPerm() => exp
            case FullPerm() => exp
            case NoPerm() => exp
            case EpsilonPerm() => exp
            case CurrentPerm(loc) => CurrentPerm(go(loc))(p, i)
            case FractionalPerm(left, right) =>
              FractionalPerm(go(left), go(right))(p, i)
            case PermDiv(left, right) =>
              PermDiv(go(left), go(right))(p, i)
            case FieldAccessPredicate(loc, perm) =>
              FieldAccessPredicate(go(loc), go(perm))(p, i)
            case PredicateAccessPredicate(loc, perm) =>
              PredicateAccessPredicate(go(loc), go(perm))(p, i)
            case fa@FuncApp(fname, args) =>
              FuncApp(fname, args map go)(p, i, fa.typ, fa.formalArgs)
            case dfa@DomainFuncApp(fname, args, m) =>
              DomainFuncApp(fname, args map go, goTypeVariables(m))(p, i, dfa.typ, dfa.formalArgs,dfa.domainName)

            case Minus(e) => Minus(go(e))(p, i)
            case Not(e) => Not(go(e))(p, i)

            case Or(l, r) => Or(go(l), go(r))(p, i)
            case And(l, r) => And(go(l), go(r))(p, i)
            case Implies(l, r) => Implies(go(l), go(r))(p, i)
            case MagicWand(l, r) => MagicWand(go(l), go(r))(p, i)

            case Add(l, r) => Add(go(l), go(r))(p, i)
            case Sub(l, r) => Sub(go(l), go(r))(p, i)
            case Mul(l, r) => Mul(go(l), go(r))(p, i)
            case Div(l, r) => Div(go(l), go(r))(p, i)
            case Mod(l, r) => Mod(go(l), go(r))(p, i)

            case LtCmp(l, r) => LtCmp(go(l), go(r))(p, i)
            case LeCmp(l, r) => LeCmp(go(l), go(r))(p, i)
            case GtCmp(l, r) => GtCmp(go(l), go(r))(p, i)
            case GeCmp(l, r) => GeCmp(go(l), go(r))(p, i)

            case EqCmp(l, r) => EqCmp(go(l), go(r))(p, i)
            case NeCmp(l, r) => NeCmp(go(l), go(r))(p, i)

            case PermMinus(e) => PermMinus(go(e))(p, i)
            case PermAdd(l, r) => PermAdd(go(l), go(r))(p, i)
            case PermSub(l, r) => PermSub(go(l), go(r))(p, i)
            case PermMul(l, r) => PermMul(go(l), go(r))(p, i)
            case IntPermMul(l, r) => IntPermMul(go(l), go(r))(p, i)

            case PermLtCmp(l, r) => PermLtCmp(go(l), go(r))(p, i)
            case PermLeCmp(l, r) => PermLeCmp(go(l), go(r))(p, i)
            case PermGtCmp(l, r) => PermGtCmp(go(l), go(r))(p, i)
            case PermGeCmp(l, r) => PermGeCmp(go(l), go(r))(p, i)

            case EmptySeq(elemTyp) => EmptySeq(go(elemTyp))(p, i)
            case ExplicitSeq(elems) => ExplicitSeq(elems map go)(p, i)
            case RangeSeq(low, high) => RangeSeq(go(low), go(high))(p, i)
            case SeqAppend(left, right) => SeqAppend(go(left), go(right))(p, i)
            case SeqIndex(seq, idx) => SeqIndex(go(seq), go(idx))(p, i)
            case SeqTake(seq, n) => SeqTake(go(seq), go(n))(p, i)
            case SeqDrop(seq, n) => SeqDrop(go(seq), go(n))(p, i)
            case SeqContains(elem, seq) => SeqContains(go(elem), go(seq))(p, i)
            case SeqUpdate(seq, idx, elem) =>
              SeqUpdate(go(seq), go(idx), go(elem))(p, i)
            case SeqLength(seq) => SeqLength(go(seq))(p, i)

            case EmptySet(elemTyp) => exp
            case ExplicitSet(elems) => ExplicitSet(elems map go)(p, i)
            case EmptyMultiset(elemTyp) => exp
            case ExplicitMultiset(elems) => ExplicitMultiset(elems map go)(p, i)
            case AnySetUnion(left, right) => AnySetUnion(go(left), go(right))(p, i)
            case AnySetIntersection(left, right) => AnySetIntersection(go(left), go(right))(p, i)
            case AnySetSubset(left, right) => AnySetSubset(go(left), go(right))(p, i)
            case AnySetMinus(left, right) => AnySetMinus(go(left), go(right))(p, i)
            case AnySetContains(elem, s) => AnySetContains(go(elem), go(s))(p, i)
            case AnySetCardinality(s) => AnySetCardinality(go(s))(p, i)
          }

        case program @
          Program(domains, fields, functions, predicates, methods) =>
          Program(domains map go, fields map go, functions map go,
            predicates map go, methods map go)(program.pos, program.info)

        case member: Member =>
          member match {
            case Domain(name, functions, axioms, typeVariables) =>
              Domain(name, functions map go, axioms map go,
                typeVariables map go)(member.pos, member.info)

            case Field(name, singleType) =>
              Field(name, go(singleType))(member.pos, member.info)

            case Function(name, parameters, aType, preconditions,
              postconditions, body) =>
              Function(name, parameters map go, go(aType),
                preconditions map go,
                postconditions map go,
                body map go)(member.pos, member.info)

            case Predicate(name, parameters, body) =>
              Predicate(name, parameters map go,
                body map go)(member.pos, member.info)

            case Method(name, parameters, results, preconditions,
              postconditions, locals, body) =>
              Method(name, parameters map go, results map go,
                preconditions map go,
                postconditions map go,
                locals map go, go(body))(member.pos, member.info)
          }

        case domainMember: DomainMember =>
          domainMember match {
            case DomainAxiom(name, body) =>
              DomainAxiom(name, go(body))(domainMember.pos,domainMember.info,domainMember.domainName)

            case DomainFunc(name, parameters, aType, unique) =>
              DomainFunc(name, parameters map go, go(aType),unique)(domainMember.pos, domainMember.info,domainMember.domainName)
          }

        case aType: Type =>
          aType match {
            case Bool => aType

            case dt@DomainType(domainName, typeVariables) =>
              DomainType(domainName, goTypeVariables(typeVariables))(dt.typeParameters)

            case Int => aType
            case Perm => aType
            case InternalType => aType
            case Wand => aType
            case Ref => aType
            case SeqType(elementType) => SeqType(go(elementType))
            case SetType(elementType) => SetType(go(elementType))
            case MultisetType(elementType) => MultisetType(go(elementType))
            case TypeVar(_) => aType
          }

        case declaration @ LocalVarDecl(name, singleType) =>
          LocalVarDecl(name, go(singleType))(declaration.pos,
            declaration.info)

        case statement: Stmt =>
          statement match {
            case Assert(expression) =>
              Assert(go(expression))(statement.pos, statement.info)

            case Exhale(expression) =>
              Exhale(go(expression))(statement.pos, statement.info)

            case FieldAssign(field, value) =>
              FieldAssign(go(field), go(value))(statement.pos, statement.info)

            case Fold(accessPredicate) =>
              Fold(go(accessPredicate))(statement.pos, statement.info)

            case Fresh(variables) =>
              Fresh(variables map go)(statement.pos, statement.info)

            case Constraining(variables, body) =>
              Constraining(
                variables map go, go(body))(statement.pos, statement.info)

            case Goto(_) => statement

            case If(condition, ifTrue, ifFalse) =>
              If(go(condition), go(ifTrue),
                go(ifFalse))(statement.pos, statement.info)

            case Inhale(expression) =>
              Inhale(go(expression))(statement.pos, statement.info)

            case Label(name, invs) => Label(name, invs map go)(statement.pos, statement.info)

            case LocalVarAssign(variable, value) =>
              LocalVarAssign(go(variable),
                go(value))(statement.pos, statement.info)

            case MethodCall(methodname, arguments, variables) =>
              MethodCall(methodname, arguments map go,
                variables map go)(statement.pos, statement.info)

            case NewStmt(target, fields) =>
              NewStmt(go(target), fields map go)(statement.pos, statement.info)

            case Seqn(statements) =>
              Seqn(statements map go)(statement.pos, statement.info)

            case Unfold(predicate) =>
              Unfold(go(predicate))(statement.pos, statement.info)

            case Package(wand) =>
              Package(go(wand))(statement.pos, statement.info)

            case Apply(wand) =>
              Apply(go(wand))(statement.pos, statement.info)

            case While(condition, invariants, locals, body) =>
              While(go(condition), invariants map go, locals map go,
                go(body))(statement.pos, statement.info)
          }

        case trigger @ Trigger(expressions) =>
         // try(
            Trigger(expressions map go)(trigger.pos, trigger.info)
         //   ) catch {
         //     case ia: IllegalArgumentException => Trigger(Seq()) (trigger.pos, trigger.info)
         //   }
        // Uninitialized subtrees are left uninitialized.
        case null => null

        case other => throw
          new AssertionError("Internal Error: transform code for node not covered: " + other.getClass)
      }
    }

    val beforeRecursion = pre.applyOrElse(node, identity[Node])
    val afterRecursion = if (recursive(node)) {
      recurse(beforeRecursion)
    } else {
      beforeRecursion
    }
    post.applyOrElse(afterRecursion, identity[Node]).asInstanceOf[A]
  }

  /**
   * Recursively transform specifications in tree rooted at `node`. This can be
   * useful to generate inhale exhale expressions.
   */
  def transformSpecifications[A <: Node](translate: Exp => Exp, node: A): A = {
    def replace: PartialFunction[Node, Node] = {
      case function @ Function(name, parameters, aType, preconditions,
        postconditions, body) =>
        Function(name, parameters map recurse, recurse(aType),
          preconditions map translate, postconditions map translate,
          body map recurse)(function.pos, function.info)

      case method @ Method(name, parameters, results, preconditions,
        postconditions, locals, body) =>
        Method(name, parameters map recurse, results map recurse,
          preconditions map translate, postconditions map translate,
          locals map recurse, recurse(body))(method.pos, method.info)

      case loop @ While(condition, invariants, locals, body) =>
        While(recurse(condition), invariants map translate,
          locals map recurse, recurse(body))(loop.pos, loop.info)

      case root @ Assert(expression) =>
        Assert(translate(expression))(root.pos, root.info)

      case exhale @ Exhale(expression) =>
        Exhale(translate(expression))(exhale.pos, exhale.info)

      case inhale @ Inhale(expression) =>
        Inhale(translate(expression))(inhale.pos, inhale.info)
    }

    def recurse[B <: Node](root: B): B = {
      transform(root, replace)()
    }
    recurse(node)
  }

  /**
   * Simplify `expression`, in particular by making use of literals. For
   * example, `!true` is replaced by `false`. Division and modulo with divisor
   * 0 are not treated. Note that an expression with non-terminating evaluation due to endless recursion
   * might be transformed to terminating expression.
   */
  def simplify(expression: Exp): Exp = {
    /* Always simplify children first, then treat parent. */
    transform(expression)(_ => true, {
      case root @ Not(BoolLit(literal)) =>
        BoolLit(!literal)(root.pos, root.info)
      case Not(Not(single)) => single

      case And(TrueLit(), right) => right
      case And(left, TrueLit()) => left
      case root @ And(FalseLit(), _) => FalseLit()(root.pos, root.info)
      case root @ And(_, FalseLit()) => FalseLit()(root.pos, root.info)

      case Or(FalseLit(), right) => right
      case Or(left, FalseLit()) => left
      case root @ Or(TrueLit(), _) => TrueLit()(root.pos, root.info)
      case root @ Or(_, TrueLit()) => TrueLit()(root.pos, root.info)

      case root @ Implies(FalseLit(), _) => TrueLit()(root.pos, root.info)
      case root @ Implies(_, TrueLit()) => TrueLit()(root.pos, root.info)
      case root @ Implies(TrueLit(), FalseLit()) =>
        FalseLit()(root.pos, root.info)
      case Implies(TrueLit(), consequent) => consequent

      case root @ EqCmp(BoolLit(left), BoolLit(right)) =>
        BoolLit(left == right)(root.pos, root.info)
      case root @ EqCmp(FalseLit(), right) => Not(right)(root.pos, root.info)
      case root @ EqCmp(left, FalseLit()) => Not(left)(root.pos, root.info)
      case EqCmp(TrueLit(), right) => right
      case EqCmp(left, TrueLit()) => left
      case root @ EqCmp(IntLit(left), IntLit(right)) =>
        BoolLit(left == right)(root.pos, root.info)
      case root @ EqCmp(NullLit(), NullLit()) => TrueLit()(root.pos, root.info)

      case root @ NeCmp(BoolLit(left), BoolLit(right)) =>
        BoolLit(left != right)(root.pos, root.info)
      case NeCmp(FalseLit(), right) => right
      case NeCmp(left, FalseLit()) => left
      case root @ NeCmp(TrueLit(), right) => Not(right)(root.pos, root.info)
      case root @ NeCmp(left, TrueLit()) => Not(left)(root.pos, root.info)
      case root @ NeCmp(IntLit(left), IntLit(right)) =>
        BoolLit(left != right)(root.pos, root.info)
      case root @ NeCmp(NullLit(), NullLit()) =>
        FalseLit()(root.pos, root.info)

      case CondExp(TrueLit(), ifTrue, _) => ifTrue
      case CondExp(FalseLit(), _, ifFalse) => ifFalse
      case root @ CondExp(_, FalseLit(), FalseLit()) =>
        FalseLit()(root.pos, root.info)
      case root @ CondExp(_, TrueLit(), TrueLit()) =>
        TrueLit()(root.pos, root.info)
      case root @ CondExp(condition, FalseLit(), TrueLit()) =>
        Not(condition)(root.pos, root.info)
      case CondExp(condition, TrueLit(), FalseLit()) => condition
      case root @ CondExp(condition, FalseLit(), ifFalse) =>
        And(Not(condition)(), ifFalse)(root.pos, root.info)
      case root @ CondExp(condition, TrueLit(), ifFalse) =>
        Or(condition, ifFalse)(root.pos, root.info)
      case root @ CondExp(condition, ifTrue, FalseLit()) =>
        And(condition, ifTrue)(root.pos, root.info)
      case root @ CondExp(condition, ifTrue, TrueLit()) =>
        Or(Not(condition)(), ifTrue)(root.pos, root.info)

      case root @ Forall(_, _, BoolLit(literal)) =>
        BoolLit(literal)(root.pos, root.info)
      case root @ Exists(_, BoolLit(literal)) =>
        BoolLit(literal)(root.pos, root.info)

      case root @ Minus(IntLit(literal)) => IntLit(-literal)(root.pos, root.info)
      case Minus(Minus(single)) => single

      case PermMinus(PermMinus(single)) => single

      case root @ GeCmp(IntLit(left), IntLit(right)) =>
        BoolLit(left >= right)(root.pos, root.info)
      case root @ GtCmp(IntLit(left), IntLit(right)) =>
        BoolLit(left > right)(root.pos, root.info)
      case root @ LeCmp(IntLit(left), IntLit(right)) =>
        BoolLit(left <= right)(root.pos, root.info)
      case root @ LtCmp(IntLit(left), IntLit(right)) =>
        BoolLit(left < right)(root.pos, root.info)

      case root @ Add(IntLit(left), IntLit(right)) =>
        IntLit(left + right)(root.pos, root.info)
      case root @ Sub(IntLit(left), IntLit(right)) =>
        IntLit(left - right)(root.pos, root.info)
      case root @ Mul(IntLit(left), IntLit(right)) =>
        IntLit(left * right)(root.pos, root.info)
      case root @ Div(IntLit(left), IntLit(right)) if right != bigIntZero =>
        IntLit(left / right)(root.pos, root.info)
      case root @ Mod(IntLit(left), IntLit(right)) if right != bigIntZero =>
        IntLit(left % right)(root.pos, root.info)
    })
  }

  private val bigIntZero = BigInt(0)
}
