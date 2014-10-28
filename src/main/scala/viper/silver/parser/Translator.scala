/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.silver.parser

import language.implicitConversions
import org.kiama.attribution.Attributable
import org.kiama.util.Messaging
import viper.silver.ast._
import viper.silver.ast.utility.{Visitor, Statements}

/**
 * Takes an abstract syntax tree after parsing is done and translates it into
 * a SIL abstract syntax tree.
 *
 * [2014-05-08 Malte] The current architecture of the resolver makes it hard
 * to detect all malformed ASTs. It is, for example, hard to detect that an
 * expression "f > 0", where f is an int-typed field, is malformed.
 * The translator can thus not assume that the input tree is completely
 * wellformed, and in cases where a malformed tree is detected, it does not
 * return a tree, but instead, records error messages using Kiama's
 * Messaging feature.
 */
case class Translator(program: PProgram) {
  val file = program.file

  def translate: Option[Program] /*(Program, Seq[Messaging.Record])*/ = {
    assert(Messaging.messagecount == 0, "Expected previous phases to succeed, but found error messages.")

    program match {
      case PProgram(f, domains, fields, functions, predicates, methods) =>
        (domains ++ fields ++ functions ++ predicates ++
          methods ++ (domains flatMap (_.funcs))) map translateMemberSignature

        val d = domains map (translate(_))
        val f = fields map (translate(_))
        val fs = functions map (translate(_))
        val p = predicates map (translate(_))
        val m = methods map (translate(_))
        val prog = Program(d, f, fs, p, m)(program.start)

        if (Messaging.messagecount == 0) Some(prog)
        else None
    }
  }

  private def translate(m: PMethod): Method = m match {
    case PMethod(name, formalArgs, formalReturns, pres, posts, body) =>
      val m = findMethod(name)
      val plocals = Visitor.deepCollect(body.childStmts, Nodes.subnodes)({ case l: PLocalVarDecl => l })
      val locals = plocals map {
        case p@PLocalVarDecl(idndef, t, _) => LocalVarDecl(idndef.name, ttyp(t))(p.start)
      }
      m.locals = locals
      m.pres = pres map exp
      m.posts = posts map exp
      m.body = stmt(body)
      m
  }

  private def translate(d: PDomain): Domain = d match {
    case PDomain(name, typVars, functions, axioms) =>
      val d = findDomain(name)
      d.functions = functions map (f => findDomainFunction(f.idndef))
      d.axioms = axioms map (translate(_))
      d
  }

  private def translate(a: PAxiom): DomainAxiom = a match {
    case PAxiom(name, e) =>
      DomainAxiom(name.name, exp(e))(a.start)
  }

  private def translate(f: PFunction) = f match {
    case PFunction(name, formalArgs, typ, pres, posts, body) =>
      val f = findFunction(name)
      f.pres = pres map exp
      f.posts = posts map exp
      f.exp = exp(body)
      f
  }

  private def translate(p: PPredicate) = p match {
    case PPredicate(name, formalArgs, body) =>
      val p = findPredicate(name)
      p.body = exp(body)
      p
  }

  private def translate(f: PField) = findField(f.idndef)

  private val members = collection.mutable.HashMap[String, Node]()
  /**
   * Translate the signature of a member, so that it can be looked up later.
   */
  private def translateMemberSignature(p: PMember) {
    val pos = p.start
    val name = p.idndef.name
    val t = p match {
      case PField(_, typ) =>
        Field(name, ttyp(typ))(pos)
      case PFunction(_, formalArgs, typ, _, _, _) =>
        Function(name, formalArgs map liftVarDecl, ttyp(typ), null, null, null)(pos)
      case PDomainFunction(_, args, typ, unique) =>
        DomainFunc(name, args map liftVarDecl, ttyp(typ), unique)(pos)
      case PDomain(_, typVars, funcs, axioms) =>
        Domain(name, null, null, typVars map (t => TypeVar(t.idndef.name)))(pos)
      case PPredicate(_, formalArgs, _) =>
        Predicate(name, formalArgs map liftVarDecl, null)(pos)
      case PMethod(_, formalArgs, formalReturns, _, _, _) =>
        Method(name, formalArgs map liftVarDecl, formalReturns map liftVarDecl, null, null, null, null)(pos)
    }
    members.put(p.idndef.name, t)
  }

  // helper methods that can be called if one knows what 'id' refers to
  private def findDomain(id: PIdentifier) = members.get(id.name).get.asInstanceOf[Domain]
  private def findField(id: PIdentifier) = members.get(id.name).get.asInstanceOf[Field]
  private def findFunction(id: PIdentifier) = members.get(id.name).get.asInstanceOf[Function]
  private def findDomainFunction(id: PIdentifier) = members.get(id.name).get.asInstanceOf[DomainFunc]
  private def findPredicate(id: PIdentifier) = members.get(id.name).get.asInstanceOf[Predicate]
  private def findMethod(id: PIdentifier) = members.get(id.name).get.asInstanceOf[Method]

  /** Takes a `PStmt` and turns it into a `Stmt`. */
  private def stmt(s: PStmt): Stmt = {
    val pos = s.start
    s match {
      case PVarAssign(idnuse, PFunctApp(func, args)) if members.get(func.name).get.isInstanceOf[Method] =>
        /* This is a method call that got parsed in a slightly confusing way.
         * TODO: Get rid of this case! There is a matching case in the resolver.
         */
        val call = PMethodCall(Seq(idnuse), func, args)
        call.setStart(s.start)
        stmt(call)
      case PVarAssign(idnuse, rhs) =>
        LocalVarAssign(LocalVar(idnuse.name)(ttyp(idnuse.typ), pos), exp(rhs))(pos)
      case PFieldAssign(field, rhs) =>
        FieldAssign(FieldAccess(exp(field.rcv), findField(field.idnuse))(field.start), exp(rhs))(pos)
      case PLocalVarDecl(idndef, t, Some(init)) =>
        LocalVarAssign(LocalVar(idndef.name)(ttyp(t), pos), exp(init))(pos)
      case PLocalVarDecl(_, _, None) =>
        // there are no declarations in the SIL AST; rather they are part of the method signature
        Statements.EmptyStmt
      case PSeqn(ss) =>
        Seqn(ss map stmt)(pos)
      case PFold(e) =>
        Fold(exp(e).asInstanceOf[PredicateAccessPredicate])(pos)
      case PUnfold(e) =>
        Unfold(exp(e).asInstanceOf[PredicateAccessPredicate])(pos)
      case PInhale(e) =>
        Inhale(exp(e))(pos)
      case PExhale(e) =>
        Exhale(exp(e))(pos)
      case PAssert(e) =>
        Assert(exp(e))(pos)
      case PNewStmt(target, fieldsOpt) =>
        val fields = fieldsOpt match {
          case None => program.fields map (translate(_))
            /* Slightly redundant since we already translated the fields when we
             * translated the PProgram at the beginning of this class.
             */
          case Some(pfields) => pfields map findField
        }
        NewStmt(exp(target).asInstanceOf[LocalVar], fields)(pos)
      case PMethodCall(targets, method, args) =>
        val ts = (targets map exp).asInstanceOf[Seq[LocalVar]]
        MethodCall(findMethod(method), args map exp, ts)(pos)
      case PLabel(name) =>
        Label(name.name)(pos)
      case PGoto(label) =>
        Goto(label.name)(pos)
      case PIf(cond, thn, els) =>
        If(exp(cond), stmt(thn), stmt(els))(pos)
      case PFresh(vars) => Fresh(vars map (v => LocalVar(v.name)(ttyp(v.typ), v.start)))(pos)
      case PConstraining(vars, ss) =>
        Constraining(vars map (v => LocalVar(v.name)(ttyp(v.typ), v.start)), stmt(ss))(pos)
      case PWhile(cond, invs, body) =>
        val plocals = body.childStmts collect {
          case l: PLocalVarDecl => l
        }
        val locals = plocals map {
          case p@PLocalVarDecl(idndef, t, _) => LocalVarDecl(idndef.name, ttyp(t))(pos)
        }
        While(exp(cond), invs map exp, locals, stmt(body))(pos)
    }
  }

  /** Takes a `PExp` and turns it into an `Exp`. */
  private def exp(pexp: PExp): Exp = {
    val pos = pexp.start
    pexp match {
      case piu @ PIdnUse(name) =>
        piu.decl match {
          case _: PLocalVarDecl | _: PFormalArgDecl => LocalVar(name)(ttyp(pexp.typ), pos)
          case pf: PField =>
            /* A malformed AST where a field is dereferenced without a receiver */
            Messaging.message(piu, s"expected expression but found field $name")
            LocalVar(pf.idndef.name)(ttyp(pf.typ), pos)
          case _ =>
            sys.error("should not occur in type-checked program")
        }
      case PBinExp(left, op, right) =>
        val (l, r) = (exp(left), exp(right))
        op match {
          case "+" =>
            r.typ match {
              case Int => Add(l, r)(pos)
              case Perm => PermAdd(l, r)(pos)
              case _ => sys.error("should not occur in type-checked program")
            }
          case "-" =>
            r.typ match {
              case Int => Sub(l, r)(pos)
              case Perm => PermSub(l, r)(pos)
              case _ => sys.error("should not occur in type-checked program")
            }
          case "*" =>
            r.typ match {
              case Int => Mul(l, r)(pos)
              case Perm =>
                l.typ match {
                  case Int => IntPermMul(l, r)(pos)
                  case Perm => PermMul(l, r)(pos)
                  case _ => sys.error("should not occur in type-checked program")
                }
              case _ => sys.error("should not occur in type-checked program")
            }
          case "/" =>
          {
            assert(r.typ==Int)
            l match {
              case lp : PermExp => PermDiv(lp, r)(pos)
              case _            => assert (l.typ==Int); FractionalPerm(l, r)(pos)
            }
          }
          case "\\" => Div(l, r)(pos)
          case "%" => Mod(l, r)(pos)
          case "<" =>
            l.typ match {
              case Int => LtCmp(l, r)(pos)
              case Perm => PermLtCmp(l, r)(pos)
              case _ => sys.error("unexpected type")
            }
          case "<=" =>
            l.typ match {
              case Int => LeCmp(l, r)(pos)
              case Perm => PermLeCmp(l, r)(pos)
              case _ => sys.error("unexpected type")
            }
          case ">" =>
            l.typ match {
              case Int => GtCmp(l, r)(pos)
              case Perm => PermGtCmp(l, r)(pos)
              case _ => sys.error("unexpected type")
            }
          case ">=" =>
            l.typ match {
              case Int => GeCmp(l, r)(pos)
              case Perm => PermGeCmp(l, r)(pos)
              case _ => sys.error("unexpected type")
            }
          case "==" => EqCmp(l, r)(pos)
          case "!=" => NeCmp(l, r)(pos)
          case "==>" => Implies(l, r)(pos)
          case "<==>" => EqCmp(l, r)(pos)
          case "&&" => And(l, r)(pos)
          case "||" => Or(l, r)(pos)
          case "in" =>
            if (right.typ.isInstanceOf[PSeqType])
              SeqContains(l, r)(pos)
            else
              AnySetContains(l, r)(pos)
          case "++" => SeqAppend(l, r)(pos)
          case "subset" => AnySetSubset(l, r)(pos)
          case "intersection" => AnySetIntersection(l, r)(pos)
          case "union" => AnySetUnion(l, r)(pos)
          case "setminus" => AnySetMinus(l, r)(pos)
          case _ => sys.error(s"unexpected operator $op")
        }
      case PUnExp(op, pe) =>
        val e = exp(pe)
        op match {
          case "+" => e
          case "-" => Neg(e)(pos)
          case "!" => Not(e)(pos)
        }
      case PInhaleExhaleExp(in, ex) =>
        InhaleExhaleExp(exp(in), exp(ex))(pos)
      case PIntLit(i) =>
        IntLit(i)(pos)
      case p@PResultLit() =>
        // find function
        var par: Attributable = p.parent
        while (!par.isInstanceOf[PFunction]) {
          if (par == null) sys.error("cannot use 'result' outside of function")
          par = par.parent
        }
        Result()(ttyp(par.asInstanceOf[PFunction].typ), pos)
      case PBoolLit(b) =>
        if (b) TrueLit()(pos) else FalseLit()(pos)
      case PNullLit() =>
        NullLit()(pos)
      case p@PFieldAccess(rcv, idn) =>
        FieldAccess(exp(rcv), findField(idn))(pos)
      case p@PPredicateAccess(args, idn) =>
        PredicateAccess(args map exp, findPredicate(idn))(pos)
      case PFunctApp(func, args) =>
        members.get(func.name).get match {
          case f: Function => FuncApp(f, args map exp)(pos)
          case f @ DomainFunc(name, formalArgs, typ, _) =>
            val actualArgs = args map exp
            val translatedTyp = ttyp(pexp.typ)
            // 'learn' looks at the formal type of an expression and the one actually
            // occurring in the program and tries to learn instantiations for type variables.
            def learn(formal: Type, actual: Type): Seq[(TypeVar, Type)] = {
              (formal, actual) match {
                case (tv: TypeVar, t) if t.isConcrete => Seq(tv -> t)
                case (DomainType(_, m1), DomainType(_, m2)) =>
                  m2.toSeq
                case _ => Nil
              }
            }
            // infer the type variable mapping for this call
            val map = learn(typ, translatedTyp) ++
              ((formalArgs map (_.typ)) zip (actualArgs map (_.typ)) flatMap (x => learn(x._1, x._2)))

            DomainFuncApp(f, actualArgs, map.toMap)(pos)
          case _ => sys.error("unexpected reference to non-function")
        }
      case PUnfolding(loc, e) =>
        Unfolding(exp(loc).asInstanceOf[PredicateAccessPredicate], exp(e))(pos)
      case PExists(vars, e) =>
        Exists(vars map liftVarDecl, exp(e))(pos)
      case PForall(vars, triggers, e) =>
        val ts = triggers map (exps => Trigger(exps map exp)(exps(0).start))
        Forall(vars map liftVarDecl, ts, exp(e))(pos)
      case POld(e) =>
        Old(exp(e))(pos)
      case PCondExp(cond, thn, els) =>
        CondExp(exp(cond), exp(thn), exp(els))(pos)
      case PCurPerm(loc) =>
        CurrentPerm(exp(loc).asInstanceOf[LocationAccess])(pos)
      case PNoPerm() =>
        NoPerm()(pos)
      case PFullPerm() =>
        FullPerm()(pos)
      case PWildcard() =>
        WildcardPerm()(pos)
      case PEpsilon() =>
        EpsilonPerm()(pos)
      case PAccPred(loc, perm) =>
        val p = exp(perm)
        exp(loc) match {
          case loc@FieldAccess(rcv, field) =>
            FieldAccessPredicate(loc, p)(pos)
          case loc@PredicateAccess(rcv, pred) =>
            PredicateAccessPredicate(loc, p)(pos)
          case _ =>
            sys.error("unexpected location")
        }
      case PEmptySeq(_) =>
        EmptySeq(ttyp(pexp.typ.asInstanceOf[PSeqType].elementType))(pos)
      case PExplicitSeq(elems) =>
        ExplicitSeq(elems map exp)(pos)
      case PRangeSeq(low, high) =>
        RangeSeq(exp(low), exp(high))(pos)
      case PSeqIndex(seq, idx) =>
        SeqIndex(exp(seq), exp(idx))(pos)
      case PSeqTake(seq, n) =>
        SeqTake(exp(seq), exp(n))(pos)
      case PSeqDrop(seq, n) =>
        SeqDrop(exp(seq), exp(n))(pos)
      case PSeqUpdate(seq, idx, elem) =>
        SeqUpdate(exp(seq), exp(idx), exp(elem))(pos)
      case PSize(s) =>
        if (s.typ.isInstanceOf[PSeqType])
          SeqLength(exp(s))(pos)
        else
          AnySetCardinality(exp(s))(pos)
      case PEmptySet(_) =>
        EmptySet(ttyp(pexp.typ.asInstanceOf[PSetType].elementType))(pos)
      case PExplicitSet(elems) =>
        ExplicitSet(elems map exp)(pos)
      case PEmptyMultiset(_) =>
        EmptyMultiset(ttyp(pexp.typ.asInstanceOf[PMultisetType].elementType))(pos)
      case PExplicitMultiset(elems) =>
        ExplicitMultiset(elems map exp)(pos)
    }
  }

  /** Takes a `scala.util.parsing.input.Position` and turns it into a `SourcePosition`. */
  implicit def liftPos(pos: scala.util.parsing.input.Position): SourcePosition = SourcePosition(file, pos.line, pos.column)

  /** Takes a `PFormalArgDecl` and turns it into a `LocalVar`. */
  private def liftVarDecl(formal: PFormalArgDecl) = LocalVarDecl(formal.idndef.name, ttyp(formal.typ))(formal.start)

  /** Takes a `PType` and turns it into a `Type`. */
  private def ttyp(t: PType): Type = t match {
    case PPrimitiv(name) => name match {
      case "Int" => Int
      case "Bool" => Bool
      case "Ref" => Ref
      case "Perm" => Perm
    }
    case PSeqType(elemType) =>
      SeqType(ttyp(elemType))
    case PSetType(elemType) =>
      SetType(ttyp(elemType))
    case PMultisetType(elemType) =>
      MultisetType(ttyp(elemType))
    case PDomainType(name, args) =>
      members.get(name.name) match {
        case Some(d) =>
          val domain = d.asInstanceOf[Domain]
          val typVarMapping = domain.typVars zip (args map ttyp)
          DomainType(domain, typVarMapping.filter {
            case (tv, tt) => !tt.isInstanceOf[TypeVar]
          }.toMap)
        case None =>
          assert(args.length == 0)
          TypeVar(name.name) // not a domain, i.e. it must be a type variable
      }
    case PUnknown() =>
      sys.error("unknown type unexpected here")
    case PPredicateType() =>
      sys.error("unexpected use of internal typ")
  }
}
