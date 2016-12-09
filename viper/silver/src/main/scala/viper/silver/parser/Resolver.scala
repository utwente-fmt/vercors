/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.silver.parser

import scala.collection.mutable
import scala.reflect._
import viper.silver.ast.MagicWandOp
import viper.silver.ast.utility.Visitor
import viper.silver.FastMessaging

/**
 * A resolver and type-checker for the intermediate SIL AST.
 */
case class Resolver(p: PProgram) {
  val names = NameAnalyser()
  val typechecker = TypeChecker(names)

  def run: Option[PProgram] = {
    if (names.run(p))
      if (typechecker.run(p))
        return Some(p)

    None
  }

  def messages = names.messages ++ typechecker.messages // ++ Consistency.messages // shouldn't be needed - Consistency errors should be generated only in later phases.
}

/**
 * Performs type-checking and sets the type of all typed nodes.
 */
case class TypeChecker(names: NameAnalyser) {

  import TypeHelper._

  var curMember: PScope = null
  var curFunction: PFunction = null
  var resultAllowed : Boolean = false

  /** to record error messages */
  var messages : FastMessaging.Messages = Nil

  def run(p: PProgram): Boolean = {
    check(p)
    messages.isEmpty
  }

  def check(p: PProgram) {
    p.domains foreach checkFunctions
    p.domains foreach checkAxioms
    p.fields foreach check
    p.functions foreach checkDeclaration
    p.predicates foreach checkDeclaration
    p.functions foreach checkBody
    p.predicates foreach checkBody
    p.methods foreach checkDeclaration
    p.methods foreach checkBody

    /* Report any domain type that couldn't be resolved */
    /* Alex suggests replacing *all* these occurrences by one arbitrary type */
    p visit {
      case dt: PDomainType if dt.isUndeclared => messages ++= FastMessaging.message(dt, s"found undeclared type ${dt.domain.name}")
    }
  }

  def checkMember(m: PScope)(fcheck: => Unit) {
    curMember = m
    fcheck
    curMember = null
  }

  def checkDeclaration(m: PMethod) {
    checkMember(m) {
      (m.formalArgs ++ m.formalReturns) foreach (a => check(a.typ))
    }
  }
  def checkBody(m: PMethod) {
    checkMember(m) {
      m.pres foreach (check(_, Bool))
      m.posts foreach (check(_, Bool))
      check(m.body)
    }
  }

  def checkDeclaration(f: PFunction) {
    checkMember(f) {
      assert(curFunction==null)
      curFunction=f
      f.formalArgs foreach (a => check(a.typ))
      check(f.typ)
      curFunction=null
    }
  }
  def checkBody(f: PFunction) {
    checkMember(f) {
      assert(curFunction==null)
      curFunction=f
      f.pres foreach (check(_, Bool))
      resultAllowed=true
      f.posts foreach (check(_, Bool))
      f.body.foreach(check(_, f.typ)) //result in the function body gets the error message somewhere else
      resultAllowed=false
      curFunction=null
    }
  }


  def checkDeclaration(p: PPredicate) {
    checkMember(p) {
      p.formalArgs foreach (a => check(a.typ))
    }
  }
  def checkBody(p: PPredicate) {
    checkMember(p) {
      p.body.foreach(check(_, Bool))
    }
  }

  def check(f: PField) {
    checkMember(f) {
      check(f.typ)
    }
  }

  def checkFunctions(d: PDomain) {
    checkMember(d) {
      d.funcs foreach check
    }
  }
  def checkAxioms(d: PDomain) {
    checkMember(d) {
      d.axioms foreach check
    }
  }

  def check(a: PAxiom) {
    checkMember(a) {
      check(a.exp, Bool)
    }
  }

  def check(f: PDomainFunction) {
    check(f.typ)
    f.formalArgs foreach (a => check(a.typ))
  }

  def check(stmt: PStmt) {
    stmt match {
      case PMacroRef(id) =>
        messages ++= FastMessaging.message(stmt, "unknown macro used: " + id.name )
      case PSeqn(ss) =>
        ss foreach check
      case PFold(e) =>
        acceptNonAbstractPredicateAccess(e, "abstract predicates cannot be folded")
        check(e, Bool)
      case PUnfold(e) =>
        acceptNonAbstractPredicateAccess(e, "abstract predicates cannot be unfolded")
        check(e, Bool)
      case PPackageWand(e) =>
        check(e,Wand)
        checkMagicWand(e, allowWandRefs = false)
      case PApplyWand(e) =>
        check(e,Wand)
        checkMagicWand(e, allowWandRefs = true)
      case PExhale(e) =>
        check(e, Bool)
      case PAssert(e) =>
        check(e, Bool)
      case PInhale(e) =>
        check(e, Bool)
      case PVarAssign(idnuse, PCall(func, args, _)) if names.definition(curMember)(func).isInstanceOf[PMethod] =>
        /* This is a method call that got parsed in a slightly confusing way.
         * TODO: Get rid of this case! There is a matching case in the translator.
         */
        val newnode: PStmt = PMethodCall(Seq(idnuse), func, args)
        newnode.setPos(stmt)
        check(newnode)

      case PVarAssign(idnuse, rhs) =>
        names.definition(curMember)(idnuse) match {
          case PLocalVarDecl(_, typ, _) =>
            check(idnuse, typ)
            check(rhs, typ)
          case PFormalArgDecl(_, typ) =>
            check(idnuse, typ)
            check(rhs, typ)
          case _ =>
            messages ++= FastMessaging.message(stmt, "expected variable as lhs")
        }
      case PNewStmt(target, fields) =>
        val msg = "expected variable as lhs"
        acceptAndCheckTypedEntity[PLocalVarDecl, PFormalArgDecl](Seq(target), msg){(v, _) => check(v, Ref)}
        fields foreach (_.foreach (field =>
          names.definition(curMember)(field, Some(PField.getClass)) match {
            case PField(_, typ) =>
              check(field, typ)
            case _ =>
              messages ++= FastMessaging.message(stmt, "expected a field as lhs")
          }))
      case PMethodCall(targets, method, args) =>
        names.definition(curMember)(method) match {
          case PMethod(_, formalArgs, formalTargets, _, _, _) =>
            formalArgs.foreach(fa=>check(fa.typ))
            if (formalArgs.length != args.length) {
              messages ++= FastMessaging.message(stmt, "wrong number of arguments")
            } else {
              if (formalTargets.length != targets.length) {
                messages ++= FastMessaging.message(stmt, "wrong number of targets")
              } else {
                for ((formal, actual) <- (formalArgs zip args) ++ (formalTargets zip targets)) {
                  check(actual, formal.typ)
                }
              }
            }
          case _ =>
            messages ++= FastMessaging.message(stmt, "expected a method")
        }
      case PLabel(name, invs) =>
        invs foreach (check(_, Bool))
      case PGoto(label) =>
        names.definition(curMember)(label) match {
          case PLabel(_, _) =>
          case _ =>
            messages ++= FastMessaging.message(stmt, "expected a label")
        }
      case PFieldAssign(field, rhs) =>
        names.definition(curMember)(field.idnuse, Some(PField.getClass)) match {
          case PField(_, typ) =>
            check(field, typ)
            check(rhs, typ)
          case _ =>
            messages ++= FastMessaging.message(stmt, "expected a field as lhs")
        }
      case PIf(cond, thn, els) =>
        check(cond, Bool)
        check(thn)
        check(els)
      case PWhile(cond, invs, body) =>
        check(cond, Bool)
        invs foreach (check(_, Bool))
        check(body)
      case PLocalVarDecl(idndef, typ, init) =>
        check(typ)
        init match {
          case Some(i) => check(i, typ)
          case None =>
        }
      case PFresh(vars) =>
        val msg = "expected variable in fresh read permission block"
        acceptAndCheckTypedEntity[PLocalVarDecl, PFormalArgDecl](vars, msg){(v, _) => check(v, Perm)}
      case PConstraining(vars, s) =>
        val msg = "expected variable in fresh read permission block"
        acceptAndCheckTypedEntity[PLocalVarDecl, PFormalArgDecl](vars, msg){(v, _) => check(v, Perm)}
        check(s)
      case plw @ PLetWand(_, wand) =>
        check(wand, Wand)
        wand match {
          case pbe: PBinExp if pbe.opName == MagicWandOp.op && pbe.typ == PWandType() => /* Good */
          case _ => messages ++= FastMessaging.message(plw, s"Expected a wand literal")
        }
      case _: PDefine =>
        /* Should have been removed right after parsing */
        sys.error(s"Unexpected node $stmt found")
      case _: PSkip =>
    }
  }

  def acceptNonAbstractPredicateAccess(exp: PExp, messageIfAbstractPredicate: String) {
    exp match {
      case PAccPred(PPredicateAccess(_, idnuse), _) =>
        acceptAndCheckTypedEntity[PPredicate, Nothing](Seq(idnuse), "expected predicate"){(_, _predicate) =>
          val predicate = _predicate.asInstanceOf[PPredicate]
          if (predicate.body.isEmpty) messages ++= FastMessaging.message(idnuse, messageIfAbstractPredicate)
        }
      case PAccPred(PCall( idnuse, _, _), _) =>
        val ad = names.definition(curMember)(idnuse)
        ad match {
          case ppa : PPredicate =>
            acceptAndCheckTypedEntity[PPredicate, Nothing](Seq(idnuse), "expected predicate"){(_, _predicate) =>
              val predicate = _predicate.asInstanceOf[PPredicate]
              if (predicate.body.isEmpty) messages ++= FastMessaging.message(idnuse, messageIfAbstractPredicate)
            }
          case _ => messages ++= FastMessaging.message(exp, "expected predicate access")
        }

      case _ => messages ++= FastMessaging.message(exp, "expected predicate access")
    }
  }

  def checkMagicWand(e: PExp, allowWandRefs: Boolean) = e match {
    case _: PIdnUse if allowWandRefs =>
    case PBinExp(_, MagicWandOp.op, _) =>
    case _ =>
      messages ++= FastMessaging.message(e, "expected magic wand")
  }

  /** This handy method checks if all passed `idnUses` refer to specific
    * subtypes `TypedEntity`s when looked up in the current scope/lookup table.
    * For each element in `idnUses`, if it refers an appropriate subtype, then
    * `handle` is applied to the current element of `idnUses` and to the
    * `TypedEntity` it refers to.
    *
    * If only a single subtype of `TypedEntity` is acceptable, pass `Nothing`
    * as the second type argument.
    *
    * Caution is advised, however, since the method checks various
    * type-relations only at runtime.
    *
    * @param idnUses Identifier usages to check
    * @param errorMessage Error message in case one of the identifiers usages
    *                     does not refer to an appropriate subtype of
    *                     `TypedEntity`
    * @param handle Handle pairs of current identifier usage and referenced
    *               `TypedEntity`
    * @tparam T1 An accepted subtype of `TypedEntity`
    * @tparam T2 Another accepted subtype of `TypedEntity`
    *
    * TODO: Generalise the method to take ClassTags T1, ..., TN.
    * TODO: If only a single T is taken, let handle be (PIdnUse, T) => Unit
    */
  def acceptAndCheckTypedEntity[T1 : ClassTag, T2 : ClassTag]
                               (idnUses: Seq[PIdnUse], errorMessage: String)
                               (handle: (PIdnUse, PTypedDeclaration) => Unit = (_, _) => ()) {

    /* TODO: Ensure that the ClassTags denote subtypes of TypedEntity */
    val acceptedClasses = Seq[Class[_]](classTag[T1].runtimeClass, classTag[T2].runtimeClass)

    idnUses.foreach { use =>
      val decl = names.definition(curMember)(use)

      acceptedClasses.find(_.isInstance(decl)) match {
        case Some(_) =>
          handle(use, decl.asInstanceOf[PTypedDeclaration])
        case None =>
          messages ++= FastMessaging.message(use, errorMessage)
      }
    }
  }

  def check(typ: PType) {
    typ match {
      case _: PPredicateType | _: PWandType =>
        sys.error("unexpected use of internal typ")
      case PPrimitiv(_) =>
      case dt@PDomainType(domain, args) =>
        args foreach check

        var x: Any = null

        try {
          x = names.definition(curMember)(domain)
        } catch {
          case _: Throwable =>
        }

        x match {
          case d@PDomain(name, typVars, _, _) =>
            ensure(args.length == typVars.length, typ, "wrong number of type arguments")
            dt.kind = PDomainTypeKinds.Domain
          case PTypeVarDecl(typeVar) =>
            dt.kind = PDomainTypeKinds.TypeVar
          case other =>
            dt.kind = PDomainTypeKinds.Undeclared
        }

      case PSeqType(elemType) =>
        check(elemType)
      case PSetType(elemType) =>
        check(elemType)
      case PMultisetType(elemType) =>
        check(elemType)
      case PUnknown() =>
        messages ++= FastMessaging.message(typ, "expected concrete type, but found unknown type")
    }
  }

    /**
   * Are types 'a' and 'b' compatible?  Type variables are assumed to be unbound so far,
   * and if they occur they are compatible with any type. PUnknown is also compatible with
   * everything, as are undeclared PDomainTypes.
   */
  def isCompatible(a: PType, b: PType): Boolean = {
    (a, b) match {
      case _ if a == b => true
      case (PUnknown(), _) | (_, PUnknown()) => true
      case (dt: PDomainType, _) if dt.isUndeclared => true
      case (_, dt: PDomainType) if dt.isUndeclared => true
      case (PTypeVar(_), _) | (_, PTypeVar(_)) => true
      case (Bool, PWandType()) => true
      case (PSeqType(e1), PSeqType(e2)) => isCompatible(e1, e2)
      case (PSetType(e1), PSetType(e2)) => isCompatible(e1, e2)
      case (PMultisetType(e1), PMultisetType(e2)) => isCompatible(e1, e2)
      case (PDomainType(domain1, args1), PDomainType(domain2, args2))
        if domain1 == domain2 && args1.length == args2.length =>
        (args1 zip args2) forall (x => isCompatible(x._1, x._2))
      case _ => false
    }
  }

  /**
   * Type-check and resolve e and ensure that it has type expected.  If that is not the case, then an
   * error should be issued.
   */
  def composeAndAdd(pts1: PTypeSubstitution,pts2: PTypeSubstitution,pt1:PType,pt2:PType) : Option[PTypeSubstitution] = {
    assert(pts1.keySet.intersect(pts2.keySet).isEmpty)
    //composed substitution before add
    val cs = new PTypeSubstitution(
      pts1.map({ case (s: String, pt: PType) => s -> pt.substitute(pts2) }) ++
        pts2.map({ case (s: String, pt: PType) => s -> pt.substitute(pts1) }))
      cs.add(pt1,pt2)
  }
  def unifySequenceWithSubstitutions(
    rlts: Set[PTypeSubstitution], //local substitutions, refreshed
    argData: scala.collection.immutable.Seq[(PType, PType, Set[PTypeSubstitution])]) : Set[PTypeSubstitution]
    // a sequence of triples, one per op arguments, where
    //_1 is the fresh local argument type
    //_2 is the type of the argument expression
    //_3 is the set of substitutions of the argument expression
      = {
      argData.foldLeft(rlts)((pss:Set[PTypeSubstitution],tri:(PType, PType, Set[PTypeSubstitution]))=>
        (for (ps:PTypeSubstitution <- pss;aps:PTypeSubstitution <- tri._3)
        yield composeAndAdd(ps,aps,tri._1,tri._2)).foldLeft(Set[PTypeSubstitution]())(
          (a:Set[PTypeSubstitution], e:Option[PTypeSubstitution])=>if (e.isDefined) a+e.get else a )
      )
    //(a:Set[PTypeSubstitution], e:Option[PTypeSubstitution])=>{
  }

  def ground(pts: PTypeSubstitution) : PTypeSubstitution =
    pts.m.flatMap(kv=>kv._2.freeTypeVariables &~ pts.m.keySet).foldLeft(pts)((ts,fv)=>ts.add(PTypeVar(fv),PTypeSubstitution.defaultType).get)

  def selectAndGroundTypeSubstitution(etss: collection.Set[PTypeSubstitution]) : PTypeSubstitution = {
    require(etss.nonEmpty)
    ground(
      if (etss.size==1)
        ground(etss.head)
      else
        etss.min(Ordering.by((ets:PTypeSubstitution)=>ets.toString)))
  }

  def typeError(exp:PExp) = {
    messages ++= FastMessaging.message(exp, s"Type error in the expression at ${exp.rangeStr}")
  }
  def check(exp: PExp, expected: PType) = checkTopTyped(exp,Some(expected))
  def checkTopTyped(exp: PExp, oexpected: Option[PType]): Unit =
  {
    check(exp,PTypeSubstitution.id)
    if (exp.typ.isValidAndResolved && exp.typeSubstitutions.nonEmpty){
      val etss = oexpected match{
        case Some(expected) if expected.isValidAndResolved => exp.typeSubstitutions.flatMap(_.add(exp.typ,expected))
        case _ => exp.typeSubstitutions
      }
      if (etss.nonEmpty) {
        val ts = selectAndGroundTypeSubstitution(etss)
        exp.forceSubstitution(ts)
      }else {
        oexpected match {
          case Some(expected) =>
            messages ++= FastMessaging.message(exp, s"Expected type ${expected.toString}, but found ${exp.typ.toString} at the expression at ${exp.rangeStr}")
          case None =>
            typeError(exp)
        }
      }
    }
  }

  def checkInternal(exp: PExp): Unit =
  {
    check(exp,PTypeSubstitution.id)
  }

  def check(exp: PExp, s : PTypeSubstitution) : Unit = {
    /**
     * Set the type of 'exp', and check that the actual type is allowed by one of the expected types.
     */
    def setType(actual: PType) {
      exp.typ = actual
    }
    /**
     * Issue an error for the node at 'n'. Also sets an error type for 'exp' to suppress
     * further warnings.
     *
     * TODO: Similar to Consistency.recordIfNot. Combine!
     */
    def issueError(n: FastPositioned, m: String) {
      messages ++= FastMessaging.message(n, m)
      setErrorType() // suppress further warnings
    }

    /**
     * Sets an error type for 'exp' to suppress further warnings.
     */
    def setErrorType() {
      setType(PUnknown())
    }

    def setPIdnUseTypeAndEntity(piu: PIdnUse, typ: PType, entity: PDeclaration) {
      setType(typ)
      piu.decl = entity
    }

    def getFreshTypeSubstitution(tvs : Set[PDomainType]) : PTypeRenaming =
      PTypeVar.freshTypeSubstitutionPTVs(tvs)


    //Checks that a substitution is fully reduced (idempotent)
    def refreshWith(ts: PTypeSubstitution, rts : PTypeRenaming) : PTypeSubstitution = {
      require(ts.isFullyReduced)
      require(rts.isFullyReduced)
//      require(rts.values.forall { case pdt: PDomainType if pdt.isTypeVar => true case _ => false })
      new PTypeSubstitution(ts map (kv => rts.rename(kv._1) -> kv._2.substitute(rts)))
    }

    var extraReturnTypeConstraint : Option[PType] = None

    exp match {
      case psl:PSimpleLiteral=>
        psl match {
          case r@PResultLit() =>
            if (resultAllowed)
              setType(curFunction.typ)
            else
              issueError(r, "'result' can only be used in function postconditions")
          case _=>
        }
      case poa: POpApp =>
        assert(poa.typeSubstitutions.isEmpty)
        poa.args.foreach(checkInternal)
        var nestedTypeError = !poa.args.forall(a => a.typ.isValidAndResolved)
        if (!nestedTypeError) {
        poa match {

          case pfa@PCall(func, args, explicitType) =>
            explicitType match {
              case Some(t) =>
                check(t)
                if (!t.isValidAndResolved) nestedTypeError = true
              case None =>
            }
            if(!nestedTypeError) {
              val ad = names.definition(curMember)(func)
                ad match {
                  case fd: PAnyFunction =>
                    pfa.function = fd
                    ensure(fd.formalArgs.size == args.size, pfa, "wrong number of arguments")
                    fd match {
                      case PFunction(_, _, resultType, _, _, _) =>
                      case pdf@PDomainFunction(_, _, resultType, unique) =>
                        val domain = names.definition(curMember)(pdf.domainName).asInstanceOf[PDomain]
                        val fdtv = PTypeVar.freshTypeSubstitution((domain.typVars map (tv => tv.idndef.name)).toSet) //fresh domain type variables
                        pfa.domainTypeRenaming = Some(fdtv)
                        pfa._extraLocalTypeVariables = (domain.typVars map (tv => PTypeVar(tv.idndef.name))).toSet
                        extraReturnTypeConstraint = explicitType
                    }
                  case ppa: PPredicate =>
                    pfa.extfunction = ppa
                    val predicate = names.definition(curMember)(func).asInstanceOf[PPredicate]
                    acceptAndCheckTypedEntity[PPredicate, Nothing](Seq(func), "expected predicate") { (id, decl) =>
                      checkInternal(id)
                      if (args.length != predicate.formalArgs.length)
                        issueError(func, "predicate arity doesn't match")
                    }
                  case x =>
                    issueError(func, "expected function or predicate ")

                }
            }
              case pue: PUnFoldingExp =>
                if (!isCompatible(pue.acc.loc.typ, Bool)) {
                  messages ++= FastMessaging.message(pue, "expected predicate access")
                }
                acceptNonAbstractPredicateAccess(pue.acc, "abstract predicates cannot be (un)folded")
              case pfa@PFieldAccess(rcv, idnuse) =>
              /* For a field access of the type rcv.fld we have to ensure that the
             * receiver denotes a local variable. Just checking that it is of type
             * Ref is not sufficient, since it could also denote a Ref-typed field.
             */
              rcv match {
                case p: PIdnUse =>
                  acceptAndCheckTypedEntity[PLocalVarDecl, PFormalArgDecl](Seq(p), "expected local variable")()
                case _ =>
                /* More complicated expressions should be ok if of type Ref, which is checked next */
              }
              acceptAndCheckTypedEntity[PField, Nothing](Seq(idnuse), "expected field")(
                (id, decl) => {
                  checkInternal(id)
                })
              case ppp@PAccPred(loc, _) =>
                loc match {
                  case ppfa@PCall(func, args, explicitType) =>
                    val ad = names.definition(curMember)(func)
                    ad match {
                      case ppf : PFunction =>
                        issueError(func, "expected predicate ")
                      case _ =>
                    }
                  case _ =>
                }
          case ppa@PPredicateAccess(args, idnuse) =>
              val predicate = names.definition(curMember)(ppa.idnuse).asInstanceOf[PPredicate]
              acceptAndCheckTypedEntity[PPredicate, Nothing](Seq(idnuse), "expected predicate") { (id, decl) =>
                checkInternal(id)
                if (args.length != predicate.formalArgs.length)
                  issueError(idnuse, "predicate arity doesn't match")
                else
                  ppa.predicate = predicate
              }
              case PPackagingGhostOp(wand, in) =>
              checkMagicWand(wand, allowWandRefs = false)
              case PApplyingGhostOp(wand, in) =>
              checkMagicWand(wand, allowWandRefs = true)
              case pecl : PEmptyCollectionLiteral =>
                check(pecl.pElementType)
              case po: POldExp =>
                // For labelled old expressions, ensure that they refer to a state label
                po match {
                  case PLabelledOld(lbl, _) =>
                    names.definition(curMember)(lbl) match {
                      case PLabel(_, _) => ()
                      case _ => messages ++= FastMessaging.message(po, "expected state label")
                    }
                  case _ => ()
                }
              case _ =>
            }

            if (poa.signatures.nonEmpty && poa.args.forall(_.typeSubstitutions.nonEmpty) && !nestedTypeError) {
              val ltr = getFreshTypeSubstitution(poa.localScope) //local type renaming - fresh versions
              val rlts = poa.signatures map (ts => refreshWith(ts, ltr)) //local substitutions refreshed
              assert(rlts.nonEmpty)
              val rrt: PDomainType = POpApp.pRes.substitute(ltr).asInstanceOf[PDomainType] // return type (which is a dummy type variable) replaced with fresh type
              val flat = poa.args.indices map (i => POpApp.pArg(i).substitute(ltr)) //fresh local argument types
              // the triples below are: (fresh argument type, argument type as used in domain of substitutions, substitutions)
              poa.typeSubstitutions ++= unifySequenceWithSubstitutions(rlts, flat.indices.map(i => (flat(i), poa.args(i).typ, poa.args(i).typeSubstitutions.toSet)) ++
                (
                  extraReturnTypeConstraint match {
                    case None => Nil
                    case Some(t) => Seq((rrt, t, Set(PTypeSubstitution.id)))
                  }
                  ))
              val ts = poa.typeSubstitutions.toSet
              if (ts.isEmpty)
                typeError(poa)
              poa.typ = if (ts.size == 1) rrt.substitute(ts.head) else rrt
            } else {
              poa.typeSubstitutions.clear()
              poa.typ = PUnknown()
            }
        }
      case piu @ PIdnUse(name) =>
        names.definition(curMember)(piu) match {
          case decl @ PLocalVarDecl(_, typ, _) => setPIdnUseTypeAndEntity(piu, typ, decl)
          case decl @ PFormalArgDecl(_, typ) => setPIdnUseTypeAndEntity(piu, typ, decl)
          case decl @ PField(_, typ) => setPIdnUseTypeAndEntity(piu, typ, decl)
          case decl @ PPredicate(_, _, _) => setPIdnUseTypeAndEntity(piu, Pred, decl)
          case decl: PLetWand => setPIdnUseTypeAndEntity(piu, Wand, decl)
          case x => issueError(piu, s"expected identifier, but got $x")
        }
      case pl@PLet(e,ns) =>
        val oldCurMember = curMember
        curMember = ns
        checkInternal(e)
        ns.variable.typ = e.typ
        checkInternal(pl.body)
        pl.typ = pl.body.typ
        pl._typeSubstitutions = (for (ts1 <- pl.body.typeSubstitutions;ts2 <- e.typeSubstitutions) yield ts1*ts2).flatten.toSet
        curMember = oldCurMember
      case pq:PQuantifier =>
        val oldCurMember = curMember
        curMember = pq
        pq.vars foreach (v => check(v.typ))
        check(pq.body,Bool)
        pq.triggers foreach (_ foreach (tpe=>checkTopTyped(tpe,None)))
        pq._typeSubstitutions = pq.body.typeSubstitutions.toSet
        pq.typ = Bool
        curMember = oldCurMember
    }
  }

  /**
   * If b is false, report an error for node.
   */
  def ensure(b: Boolean, node: FastPositioned, msg: String) {
    if (!b) messages ++= FastMessaging.message(node, msg)
  }
}

/**
 * Resolves identifiers to their declaration.
 */
case class NameAnalyser() {

  /** To record error messages */
  var messages : FastMessaging.Messages = Nil


  /** Resolves the entity to which the given identifier `idnuse` refers.
    *
    * If `member` is not null then the identifier will first be looked up in
    * the scope defined by the member. If it fails (or if the member is null),
    * the wider scope will be considered.
    *
    * In order to resolve name clashes, e.g., if the identifier is expected to
    * refer to a field, but there is a local variable with the same name in the
    * member scope that shadows the field, then the `expected` class can be
    * provided (e.g., `PField`), with the result that the shadowing local
    * variable will be ignored because its class (`PLocalVarDecl`) doesn't
    * match.
    *
    * @param member Current scope in which to start the resolving.
    * @param idnuse Identifier that is to be resolved.
    * @param expected Expected class of the entity.
    * @return Resolved entity.
    */
  def definition(member: PScope)(idnuse: PIdnUse, expected: Option[Class[_]] = None): PDeclaration = {
    if (member == null) {
      globalDeclarationMap.get(idnuse.name).get.asInstanceOf[PDeclaration]
    } else {
      // lookup in method map first, and otherwise in the general one
      val entity =
        localDeclarationMaps.get(member.scopeId).get.get(idnuse.name) match {
          case None =>
            globalDeclarationMap.get(idnuse.name).get
          case Some(foundEntity) =>
            if (expected.isDefined && foundEntity.getClass != expected.get)
              globalDeclarationMap.get(idnuse.name).get
            else
              foundEntity
        }

      entity.asInstanceOf[PDeclaration] // TODO: Why is the cast necessary? Remove if possible.
    }
  }

  def reset() {
    globalDeclarationMap.clear()
    localDeclarationMaps.clear()
  }

  private val globalDeclarationMap = mutable.HashMap[String, PEntity]()

  /* [2014-11-13 Malte] Changed localDeclarationMaps to be a map from PScope.Id
   * instead of from PScope directly. This was necessary in order to support
   * changing PScopes during type-checking, e.g., when changing the type of a
   * variable bound by a let-expression. This change (potentially) affects the
   * hashcode of the let-expression (which is a PScope), which in turn affects
   * localDeclarationMaps because such that the value stored for scope cannot
   * be retrieved anymore.
   */
  private val localDeclarationMaps = mutable.HashMap[PScope.Id, mutable.HashMap[String, PEntity]]()

  def run(p: PProgram): Boolean = {
    var curMember: PScope = null
    def getMap(d:PNode) : mutable.HashMap[String, PEntity] =
      d match {
        case d: PGlobalDeclaration => globalDeclarationMap
        case _ => getCurrentMap
      }
    def getCurrentMap: mutable.HashMap[String, PEntity] =
      if (curMember == null) globalDeclarationMap else localDeclarationMaps.get(curMember.scopeId).get

    val scopeStack = mutable.Stack[PScope]()

    val nodeDownNameCollectorVisitor = new PartialFunction[PNode,Unit] {
      def apply(n:PNode) = {
        n match {
          case d: PDeclaration =>
            getMap(d).get(d.idndef.name) match {
              case Some(e: PDeclaration) =>
                messages ++= FastMessaging.message(e, "Duplicate identifier `" + e.idndef.name + "' at " + e.idndef.start + " and at " + d.idndef.start)
              case Some(e:PErrorEntity) =>
              case None =>
                globalDeclarationMap.get(d.idndef.name) match {
                  case Some(e: PDeclaration) =>
                    messages ++= FastMessaging.message(e, "Identifier shadowing `" + e.idndef.name + "' at " + e.idndef.start + " and at " + d.idndef.start)
                  case Some(e:PErrorEntity) =>
                  case None =>
                    getMap(d).put(d.idndef.name, d)
                }
            }
          case _ =>
        }

        n match {
          case s: PScope =>
            val localDeclarations =
              if (curMember == null)
                mutable.HashMap[String, PEntity]()
              else
                localDeclarationMaps.getOrElse(curMember.scopeId, mutable.HashMap[String, PEntity]()).clone()

            localDeclarationMaps.put(s.scopeId, localDeclarations)
            scopeStack.push(curMember)
            curMember = s
          case _ =>
        }
      }

      def isDefinedAt(n:PNode) = {
        n match {
          case d: PDeclaration => true
          case s: PScope => true
          case _ => false
        }
      }
    }

    val nodeUpNameCollectorVisitor = new PartialFunction[PNode,Unit] {
      def apply(n:PNode) = {
        n match {
          case _: PScope =>
            curMember = scopeStack.pop()
          case _ =>
        }
      }
      def isDefinedAt(n:PNode) = {
        n match {
          case s: PScope => true
          case _ => false
        }
      }
    }

    def containsSubnode(container : PNode, toFind : PNode) : Boolean = {
      Visitor.existsDefined(container, Nodes.subnodes){ case found if (found eq toFind) && found != container => }
    }

    def containsSubnodeBefore(container: PNode, toFind: PNode, before: PNode) : Boolean = {
      var beforeFound = false
      val pred = new PartialFunction[PNode, PNode] {
        def isDefinedAt(node: PNode): Boolean = {
          if (!beforeFound){
            if (node eq before){
              beforeFound = true
            }
          }
          (node eq toFind) && node != container && !beforeFound
        }

        def apply(node: PNode) = node
      }
      Visitor.existsDefined(container, Nodes.subnodes)(pred)
    }

    def getContainingMethod(node : PNode) : Option[PMethod] = {
      node match {
        case null => None
        case method : PMethod => Some(method)
        case nonMethod =>
          nonMethod.parent match {
            case parentNode : PNode => getContainingMethod(parentNode)
            case _ => None
          }
      }
    }

    // find all declarations
    p.visit( nodeDownNameCollectorVisitor,nodeUpNameCollectorVisitor)

    /* Check all identifier uses. */
    p.visit({
      case m: PScope =>
        scopeStack.push(curMember)
        curMember = m
      case i@PIdnUse(name) =>
        // look up in both maps (if we are not in a method currently, we look in the same map twice, but that is ok)
        getCurrentMap.getOrElse(name, globalDeclarationMap.getOrElse(name, PUnknownEntity())) match {
          case PUnknownEntity() =>
            // domain types can also be type variables, which need not be declared
            if (!i.parent.isInstanceOf[PDomainType]) {
              messages ++= FastMessaging.message(i, s"identifier $name not defined.")
            }
          case localVar : PLocalVarDecl =>
            getContainingMethod(localVar) match {
              case Some(PMethod(_, args, returns, pres, posts, body)) =>
                // local variables must not be used in pre- or postconditions
                // see Silver issue #56
                if (pres.exists(pre => containsSubnode(pre, i)) || posts.exists(post => containsSubnode(post, i))){
                  messages ++= FastMessaging.message(i, s"local variable $name cannot be accessed in pre- or postcondition.")
                }
                // Variables must not be used before they are declared
                // This is a workaround that should work for most cases, but not all, but should be alright
                // until scopes are supported properly. E.g. it does not prevent using a variable in an else clause
                // if it has been defined in the respective then-clause.
                // See Silver issue #116
                if (containsSubnodeBefore(body, i, localVar)){
                  messages ++= FastMessaging.message(i, s"local variable $name cannot be accessed before it is declared.")
                }
              case _ =>
            }
          // return parameters must not be used in preconditions
          // see Silver issue #77
          case arg : PFormalArgDecl =>
            getContainingMethod(arg) match {
              case Some(PMethod(_, args, returns, pres, posts, _)) =>
                if (returns.contains(arg) && pres.exists(pre => containsSubnode(pre, i))){
                  messages ++= FastMessaging.message(i, s"return variable $name cannot be accessed in precondition.")
                }
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }, {
      case m: PScope =>
        curMember = scopeStack.pop()
      case _ =>
    })

    messages.isEmpty
  }
}
