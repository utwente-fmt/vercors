package viper.api.transform

import hre.util.ScopedStack
import vct.col.ast.{PredicateLocation, SilverFieldLocation}
import vct.col.origin.{AccountedDirection, FailLeft, FailRight, Name}
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers.unfoldStar
import vct.col.{ast => col}
import vct.result.VerificationError.{SystemError, Unreachable}
import viper.silver.ast.TypeVar
import viper.silver.plugin.standard.termination.{DecreasesClause, DecreasesTuple, DecreasesWildcard}
import viper.silver.{ast => silver}

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ColToSilver {
  def transform(program: col.Program[_]): (silver.Program, Map[Int, col.Node[_]]) = {
    val cts = ColToSilver(program)
    val p = cts.transform()
    (p, cts.nodeFromUniqueId.toMap)
  }
}

case class ColToSilver(program: col.Program[_]) {
  val domains: ArrayBuffer[silver.Domain] = ArrayBuffer()
  val fields: mutable.Map[col.SilverField[_], silver.Field] = mutable.Map()
  val functions: ArrayBuffer[silver.Function] = ArrayBuffer()
  val predicates: ArrayBuffer[silver.Predicate] = ArrayBuffer()
  val methods: ArrayBuffer[silver.Method] = ArrayBuffer()

  val nameStack: mutable.Stack[mutable.Map[col.Declaration[_], (String, Int)]] = mutable.Stack()
  var names: mutable.Map[col.Declaration[_], (String, Int)] = mutable.Map()
  val currentPredicatePath: ScopedStack[Seq[AccountedDirection]] = ScopedStack()
  val currentInvariant: ScopedStack[col.LoopInvariant[_]] = ScopedStack()
  val currentStarall: ScopedStack[col.Starall[_]] = ScopedStack()
  val currentUnfolding: ScopedStack[col.Unfolding[_]] = ScopedStack()
  val currentMapGet: ScopedStack[col.MapGet[_]] = ScopedStack()
  val currentDividingExpr: ScopedStack[col.DividingExpr[_]] = ScopedStack()

  val inTriggers: ScopedStack[Unit] = ScopedStack()

  case class NotSupported(node: col.Node[_]) extends SystemError {
    override def text: String =
      program.highlight(node).messageInContext(s"This kind of node (${node.getClass.getSimpleName}) is not supported by silver directly. Is there a rewrite missing?")
  }

  def ??(node: col.Node[_]): Nothing =
    throw NotSupported(node)

  def push(): Unit = nameStack.push(names.clone())
  def pop(): Unit = names = nameStack.pop()

  def unpackName(name: String): (String, Int) = {
    val m = "^(.*?)([1-9][0-9]*)?$".r.findFirstMatchIn(name).get
    if(Option(m.group(2)).isDefined) {
      (m.group(1), Integer.parseInt(m.group(2)))
    } else {
      (m.group(1), 0)
    }
  }

  def packName(name: String, index: Int): String =
    if(index == 0) name
    else s"$name$index"


  def sanitize(name: String): String = {
    if(name.isEmpty) return "_"

    val sanitized = name.flatMap {
      case '$' => "$"
      case '_' => "_"
      case c if c.isLetterOrDigit => c.toString

      // Add as desired
      case '+' => "plus"
      case '-' => "minus"
      case '@' => "at"
      case '\'' => "prime"

      case _ => "_"
    }

    sanitized.head match {
      case c if c.isLetter => sanitized
      case '_' | '$' => sanitized
      case _ => "_" + sanitized
    }
  }

  /**
   * Give the declaration a silver-appropriate name that is as close as possible to the preferred name
   */
  def name(decl: col.Declaration[_], nameF: Name => String): String =
    if(names.contains(decl)) {
      ???
    } else {
      var (name, index) = unpackName(nameF(decl.o.getPreferredNameOrElse()))
      name = sanitize(name)
      while(names.values.exists(_ == (name, index)) || silver.utility.Consistency.reservedNames.contains(packName(name, index))) {
        index += 1
      }
      names(decl) = (name, index)
      packName(name, index)
    }

  /**
   * Evaluate f within a new scope
   */
  def scoped[T](f: => T): T = {
    push()
    val result = f
    pop()
    result
  }

  /**
   * Name decl in the current scope, then evaluate f within a new scopegroupCount
   */
  def scoped[T](decl: col.Declaration[_], nameF: Name => String)(f: => T): T = {
    name(decl, nameF)
    scoped(f)
  }

  /**
   * Retrieve the name for this reference
   */
  def ref[G](r: Ref[G, _ <: col.Declaration[G]]): String = ref(r.decl)

  /**
   * Retrieve the name for this declaration
   */
  def ref(decl: col.Declaration[_]): String =
    if(names.contains(decl)) {
      val (name, index) = names(decl)
      packName(name, index)
    } else {
      throw Unreachable(s"Declaration was not yet named: $decl")
    }

  var uniquePosId: Int = 0
  val nodeFromUniqueId: mutable.Map[Int, col.Node[_]] = mutable.Map()

  def pos(node: col.Node[_]): silver.Position = {
    uniquePosId += 1
    nodeFromUniqueId(uniquePosId) = node
    // Replace : with -, as the colon interferes with z3's quantifier statistics output, which uses a colon as a separator
    silver.VirtualPosition(s"${node.o.shortPositionText.replace(':', '-')};unique_id=$uniquePosId")
  }

  def transform(): silver.Program = {
    program.declarations.foreach {
      case decl: col.AxiomaticDataType[_] => name(decl, _.ucamel)
      case decl: col.ProverType[_] => name(decl, _.ucamel)
      case decl => name(decl, _.camel)
    }
    program.declarations.collect { case adt: col.AxiomaticDataType[_] => adt }.flatMap(_.typeArgs).foreach(name(_, _.usnake))
    program.declarations.collect { case adt: col.AxiomaticDataType[_] => adt }.flatMap(_.decls).collect { case func: col.ADTFunction[_] => func }.foreach(name(_, _.snake))
    program.declarations.collect { case field: col.SilverField[_] => field }.foreach(field => fields(field) = silver.Field(ref(field), typ(field.t))(pos=pos(field), info=NodeInfo(field)))
    program.declarations.foreach(collect)
    silver.Program(domains.toSeq, fields.values.toSeq, functions.toSeq, predicates.toSeq, methods.toSeq, extensions=Seq())()
  }

  def collect(decl: col.GlobalDeclaration[_]): Unit = decl match {
    case t: col.ProverType[_] =>
      domains += silver.Domain(
        name = ref(t),
        typVars = Nil,
        functions = Nil,
        axioms = Nil,
        interpretations = Some(interpretations(t.interpretation)),
      )(pos=pos(t), info=NodeInfo(t))
    case function: col.ProverFunction[_] =>
      val domainName = "$domain$" + ref(function)
      domains += silver.Domain(
        name = domainName,
        typVars = Nil,
        axioms = Nil,
        functions = Seq(silver.DomainFunc(
          name = ref(function),
          formalArgs = function.args.map(variable),
          typ = typ(function.returnType),
          unique = false,
          interpretation = function.interpretation.collectFirst { case col.SmtLib() -> int => int },
        )(pos=pos(function), info=NodeInfo(function), domainName=domainName)),
      )(pos=pos(function), info=NodeInfo(function))
    case field: col.SilverField[_] =>
      // nop
    case rule: col.SimplificationRule[_] =>
      ??(rule)
    case function: col.Function[_] if !function.inline && function.typeArgs.isEmpty=>
      scoped {
        functions += silver.Function(
          ref(function),
          function.args.map(variable),
          typ(function.returnType),
          pred(function.contract.requires) ++ function.contract.decreases.toSeq.map(decreases),
          pred(function.contract.ensures),
          function.body.map(exp),
        )(pos=pos(function), info=NodeInfo(function))
      }
    case procedure: col.Procedure[_] if procedure.returnType == col.TVoid() && !procedure.inline && !procedure.pure && procedure.typeArgs.isEmpty =>
      scoped {
        val labelDecls = procedure.body.toSeq.flatMap(_.transSubnodes.collect {
          case l: col.LabelDecl[_] => silver.Label(name(l, _.usnake), Seq())(pos=pos(l), info=NodeInfo(l))
        })
        methods += silver.Method(
          ref(procedure),
          procedure.args.map(variable),
          procedure.outArgs.map(variable),
          pred(procedure.contract.requires) ++ procedure.contract.decreases.toSeq.map(decreases),
          pred(procedure.contract.ensures),
          procedure.body.map(body => silver.Seqn(Seq(block(body)), labelDecls)(pos=pos(body), info=NodeInfo(body)))
        )(pos=pos(procedure), info=NodeInfo(procedure))
      }
    case predicate: col.Predicate[_] if !predicate.inline && !predicate.threadLocal =>
      scoped {
        predicates += silver.Predicate(
          ref(predicate),
          predicate.args.map(variable),
          predicate.body.map(exp)
        )(pos=pos(predicate), info=NodeInfo(predicate))
      }
    case adt: col.AxiomaticDataType[_] =>
      domains += silver.Domain(
        name = ref(adt),
        typVars = adt.typeArgs.map(v => silver.TypeVar(ref(v))),
        functions = adt.decls.collect {
          case func: col.ADTFunction[_] => scoped {
            silver.DomainFunc(ref(func), func.args.map(variable), typ(func.returnType), unique = false)(pos = pos(func), info = NodeInfo(func), domainName = ref(adt))
          }
        },
        axioms = adt.decls.collect {
          case ax: col.ADTAxiom[_] =>
            silver.AnonymousDomainAxiom(exp(ax.axiom))(pos=pos(ax), info=NodeInfo(ax), domainName=ref(adt))
        },
      )(pos=pos(adt), info=NodeInfo(adt))
    case other =>
      ??(other)
  }

  def decreases(clause: col.DecreasesClause[_]): DecreasesClause = clause match {
    case col.DecreasesClauseAssume() => DecreasesWildcard(condition = None)(pos=pos(clause), info=NodeInfo(clause))
    case col.DecreasesClauseNoRecursion() => DecreasesTuple(Nil, condition = None)(pos=pos(clause), info=NodeInfo(clause))
    case col.DecreasesClauseTuple(exprs) => DecreasesTuple(exprs.map(exp), condition = None)(pos=pos(clause), info=NodeInfo(clause))
  }

  def interpretations(interpretation: Seq[(col.ProverLanguage[_], String)]) =
    interpretation.map {
      case col.SmtLib() -> int => "SMTLIB" -> int
      case col.Boogie() -> int => "Boogie" -> int
    }.to(ListMap)

  def variable(v: col.Variable[_]): silver.LocalVarDecl =
    silver.LocalVarDecl(name(v, _.camel), typ(v.t))(pos=pos(v), info=NodeInfo(v))

  def adtTypeArgs(adt: col.AxiomaticDataType[_]): Seq[TypeVar] =
    adt.typeArgs.map(v => silver.TypeVar(ref(v)))

  def typ(t: col.Type[_]): silver.Type = t match {
    case col.TBool() => silver.Bool
    case col.TInt() => silver.Int
    case col.TRational() => silver.Perm
    case col.TRef() => silver.Ref
    case col.TSeq(element) => silver.SeqType(typ(element))
    case col.TSet(element) => silver.SetType(typ(element))
    case col.TBag(element) => silver.MultisetType(typ(element))
    case col.TMap(key, value) => silver.MapType(typ(key), typ(value))
    case col.TVar(Ref(v)) => silver.TypeVar(ref(v))
    case col.TAxiomatic(Ref(adt), args) =>
      val typeArgs = adtTypeArgs(adt)
      silver.DomainType(ref(adt), ListMap(typeArgs.zip(args.map(typ)) : _*))(typeArgs)
    case col.TProverType(Ref(t)) =>
      silver.BackendType(ref(t), interpretations(t.interpretation))
    case other => ??(other)
  }

  def pred(e: col.AccountedPredicate[_], path: Seq[AccountedDirection] = Nil): Seq[silver.Exp] = e match {
    case col.UnitAccountedPredicate(pred) => currentPredicatePath.having(path) { unfoldStar(pred).map(exp) }
    case col.SplitAccountedPredicate(left, right) => pred(left, path :+ FailLeft) ++ pred(right, path :+ FailRight)
  }

  def expInfo[T <: col.Node[_]](e: T): NodeInfo[T] = {
    val result = NodeInfo(e)
    result.predicatePath = currentPredicatePath.topOption
    result.invariant = currentInvariant.topOption
    result.starall = currentStarall.topOption
    result.unfolding = currentUnfolding.topOption
    result.mapGet = currentMapGet.topOption
    result.dividingExpr = currentDividingExpr.topOption
    result
  }

  def exp(e: col.Expr[_]): silver.Exp = e match {
    case col.BooleanValue(value) => silver.BoolLit(value)(pos=pos(e), info=expInfo(e))
    case col.IntegerValue(value) => silver.IntLit(value)(pos=pos(e), info=expInfo(e))
    case col.SilverNull() => silver.NullLit()(pos=pos(e), info=expInfo(e))
    case col.Result(Ref(app)) => silver.Result(typ(app.returnType))(pos=pos(e), info=expInfo(e))

    case col.NoPerm() => silver.NoPerm()(pos=pos(e), info=expInfo(e))
    case col.WritePerm() => silver.FullPerm()(pos=pos(e), info=expInfo(e))

    case col.LiteralSeq(t, Nil) => silver.EmptySeq(typ(t))(pos=pos(e), info=expInfo(e))
    case col.LiteralSet(t, Nil) => silver.EmptySet(typ(t))(pos=pos(e), info=expInfo(e))
    case col.LiteralBag(t, Nil) => silver.EmptyMultiset(typ(t))(pos=pos(e), info=expInfo(e))
    case col.LiteralMap(k, v, Nil) => silver.EmptyMap(typ(k), typ(v))(pos=pos(e), info=expInfo(e))
    case col.LiteralSeq(_, xs) => silver.ExplicitSeq(xs.map(exp))(pos=pos(e), info=expInfo(e))
    case col.LiteralSet(_, xs) => silver.ExplicitSet(xs.map(exp))(pos=pos(e), info=expInfo(e))
    case col.LiteralBag(_, xs) => silver.ExplicitMultiset(xs.map(exp))(pos=pos(e), info=expInfo(e))
    case col.LiteralMap(_, _, xs) => silver.ExplicitMap(xs.map { case (k, v) => silver.Maplet(exp(k), exp(v))(pos=pos(e), info=expInfo(e)) })(pos=pos(e), info=expInfo(e))

    case col.SilverSeqSize(obj) => silver.SeqLength(exp(obj))(pos=pos(e), info=expInfo(e))
    case col.SilverSetSize(obj) => silver.AnySetCardinality(exp(obj))(pos=pos(e), info=expInfo(e))
    case col.SilverBagSize(obj) => silver.AnySetCardinality(exp(obj))(pos=pos(e), info=expInfo(e))
    case col.SilverMapSize(obj) => silver.MapCardinality(exp(obj))(pos=pos(e), info=expInfo(e))

    case col.Exists(bindings, triggers, body) =>
      scoped { silver.Exists(bindings.map(variable), triggers.map(trigger), exp(body))(pos=pos(e), info=expInfo(e)) }
    case col.Forall(bindings, triggers, body) =>
      scoped { silver.Forall(bindings.map(variable), triggers.map(trigger), exp(body))(pos=pos(e), info=expInfo(e)) }
    case starall @ col.Starall(bindings, triggers, body) =>
      scoped {
        currentStarall.having(starall) {
          val newBindings = bindings.map(variable)
          val newTriggers = inTriggers.having(()) {
             triggers.map(trigger)
          }
          val foralls: Seq[silver.Forall] = silver.utility.QuantifiedPermissions.desugarSourceQuantifiedPermissionSyntax(
            silver.Forall(newBindings, newTriggers, exp(body))(pos=pos(e), info=expInfo(e))
          )

          foralls.reduce[silver.Exp] { case (l, r) => silver.And(l, r)(pos=pos(e), info=expInfo(e)) }
        }
      }
    case col.Let(binding, value, main) =>
      scoped { silver.Let(variable(binding), exp(value), exp(main))(pos=pos(e), info=expInfo(e)) }
    case col.ForPerm(bindings, loc, body) =>
      scoped {
        silver.ForPerm(bindings.map(variable), loc match {
          case SilverFieldLocation(obj, field) => silver.FieldAccess(exp(obj), fields(field.decl))(pos=pos(loc), info=expInfo(obj))
          case PredicateLocation(predicate, args) => silver.PredicateAccess(args.map(exp), ref(predicate))(pos=pos(loc), info=expInfo(e))
          case other => ??(other)
        }, exp(body))(pos=pos(e), info=expInfo(e))
      }
    case col.Not(arg) => silver.Not(exp(arg))(pos=pos(e), info=expInfo(e))
    case col.And(left, right) => silver.And(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.Star(left, right) => silver.And(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.Implies(left, right) => silver.Implies(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.Or(left, right) => silver.Or(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.PolarityDependent(onInhale, onExhale) => silver.InhaleExhaleExp(exp(onInhale), exp(onExhale))(pos=pos(e), info=expInfo(e))

    case res @ col.Perm(col.SilverFieldLocation(obj, Ref(field)), perm) =>
      val permValue = exp(perm)
      permValue.info.asInstanceOf[NodeInfo[_]].permissionValuePermissionNode = Some(res)
      silver.FieldAccessPredicate(silver.FieldAccess(exp(obj), fields(field))(pos=pos(res), info=expInfo(res)), permValue)(pos=pos(res), info=expInfo(res))
    case res: col.PredicateApply[_] =>
      if(inTriggers.nonEmpty){
        return predInTrigger(res)
      }
      val silver = pred(res)
      silver.perm.info.asInstanceOf[NodeInfo[_]].permissionValuePermissionNode = Some(res)
      silver
    case col.Wand(left, right) => silver.MagicWand(exp(left), exp(right))(pos = pos(e), info=expInfo(e))
    case col.CurPerm(loc) => loc match {
      case col.PredicateLocation(predicate, args) => silver.CurrentPerm(silver.PredicateAccess(args.map(exp), ref(predicate))(pos=pos(e), info=expInfo(e)))(pos=pos(e), info=expInfo(e))
      case col.SilverFieldLocation(obj, field) => silver.CurrentPerm(silver.FieldAccess(exp(obj), fields(field.decl))(pos=pos(e), info=expInfo(e)))(pos=pos(e), info=expInfo(e))
      case default => ??(default)
    }
    case col.Value(loc) => loc match {
      case col.SilverFieldLocation(obj, field) =>
        silver.FieldAccessPredicate(silver.FieldAccess(exp(obj), fields(field.decl))(pos=pos(loc), info=expInfo(e)), silver.WildcardPerm()())(pos=pos(e), info=expInfo(e))
      case col.PredicateLocation(predicate, args) =>
        silver.PredicateAccessPredicate(silver.PredicateAccess(args.map(exp), ref(predicate))(pos = pos(loc), NodeInfo(loc)), silver.WildcardPerm()())(pos = pos(e), expInfo(e))
      case default => ??(default)
    }
    case col.Local(v) => silver.LocalVar(ref(v), typ(v.decl.t))(pos=pos(e), info=expInfo(e))
    case col.SilverDeref(obj, ref) => silver.FieldAccess(exp(obj), fields(ref.decl))(pos=pos(e), info=expInfo(e))
    case col.FunctionInvocation(f, args, Nil, Nil, Nil) =>
      silver.FuncApp(ref(f), args.map(exp))(pos(e), expInfo(e), typ(f.decl.returnType), silver.NoTrafos)
    case inv @ col.ADTFunctionInvocation(typeArgs, Ref(func), args) => typeArgs match {
      case Some((Ref(adt), typeArgs)) =>
        silver.DomainFuncApp(ref(func), args.map(exp), ListMap(adtTypeArgs(adt).zip(typeArgs.map(typ)) : _*))(pos(e), expInfo(e), typ(inv.t), ref(adt), silver.NoTrafos)
      case None => ??(inv)
    }
    case inv @ col.ProverFunctionInvocation(Ref(func), args) =>
      silver.BackendFuncApp(ref(func), args.map(exp))(pos(e), expInfo(e), typ(func.returnType), func.interpretation.collectFirst { case col.SmtLib() -> int => int }.get, silver.NoTrafos)
    case u @ col.Unfolding(p: col.PredicateApply[_], body) =>
      silver.Unfolding(
        currentUnfolding.having(u) { pred(p) },
        exp(body))(pos=pos(e), info=expInfo(e))
    case col.Select(condition, whenTrue, whenFalse) => silver.CondExp(exp(condition), exp(whenTrue), exp(whenFalse))(pos=pos(e), info=expInfo(e))
    case col.Old(expr, None) => silver.Old(exp(expr))(pos=pos(e), info=expInfo(e))
    case col.Old(expr, Some(lbl)) => silver.LabelledOld(exp(expr), ref(lbl))(pos=pos(e), info=expInfo(e))

    case col.UMinus(arg) => silver.Minus(exp(arg))(pos=pos(e), info=expInfo(e))

    case op @ col.Plus(left, right) if op.isIntOp => silver.Add(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case op @ col.Minus(left, right) if op.isIntOp => silver.Sub(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case op @ col.Mult(left, right) if op.isIntOp => silver.Mul(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case op @ col.Mod(left, right) if op.isIntOp =>
      currentDividingExpr.having(op) {
        silver.Mod(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
      }

    case op @ col.Plus(left, right) if !op.isIntOp => silver.PermAdd(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case op @ col.Minus(left, right) if !op.isIntOp => silver.PermSub(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case op @ col.Mult(left, right) if !op.isIntOp => silver.PermMul(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case op @ col.Mod(left, right) if !op.isIntOp => ??(op)

    case div @ col.RatDiv(left, right) if right.t.isInstanceOf[col.TRational[_]] =>
      currentDividingExpr.having(div) {
        silver.PermPermDiv(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
      }
    case div @ col.RatDiv(left, right) =>
      currentDividingExpr.having(div) {
        silver.PermDiv(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
      }
    case div @ col.FloorDiv(left, right) =>
      currentDividingExpr.having(div) {
        silver.Div(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
      }

    case col.SilverIntToRat(col.NoPerm()) => silver.NoPerm()(pos=pos(e), info=expInfo(e))
    case col.SilverIntToRat(col.WritePerm()) => silver.FullPerm()(pos=pos(e), info=expInfo(e))
    case col.SilverIntToRat(perm) => silver.IntPermMul(exp(perm), silver.FullPerm()(pos=pos(e), info=expInfo(e)))(pos=pos(e), info=expInfo(e))

    case col.Eq(left, right) => silver.EqCmp(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.Neq(left, right) => silver.NeCmp(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.Greater(left, right) => silver.GtCmp(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.Less(left, right) => silver.LtCmp(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.GreaterEq(left, right) => silver.GeCmp(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.LessEq(left, right) => silver.LeCmp(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.SubSetEq(left, right) => silver.AnySetSubset(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.SubBagEq(left, right) => silver.AnySetSubset(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.MapEq(left, right) => silver.EqCmp(exp(left), exp(right))(pos=pos(e), info=expInfo(e))
    case col.MapDisjoint(left, right) => ???

    case subscript@col.SeqSubscript(seq, index) =>
      val silverIndex = exp(index)
      silverIndex.info.asInstanceOf[NodeInfo[_]].seqIndexSubscriptNode = Some(subscript)
      silver.SeqIndex(exp(seq), silverIndex)(pos=pos(e), info=expInfo(e))
    case col.Range(from, to) => silver.RangeSeq(exp(from), exp(to))(pos=pos(e), info=expInfo(e))
    case col.Drop(xs, count) => silver.SeqDrop(exp(xs), exp(count))(pos=pos(e), info=expInfo(e))
    case col.Take(xs, count) => silver.SeqTake(exp(xs), exp(count))(pos=pos(e), info=expInfo(e))
    case col.SeqUpdate(xs, i, x) => silver.SeqUpdate(exp(xs), exp(i), exp(x))(pos=pos(e), info=expInfo(e))
    case col.Concat(xs, ys) => silver.SeqAppend(exp(xs), exp(ys))(pos=pos(e), info=expInfo(e))

    case col.SetMember(x, xs) => silver.AnySetContains(exp(x), exp(xs))(pos=pos(e), info=expInfo(e))
    case col.SeqMember(x, xs) => silver.SeqContains(exp(x), exp(xs))(pos=pos(e), info=expInfo(e))
    case col.BagMemberCount(x, xs) => silver.AnySetContains(exp(x), exp(xs))(pos=pos(e), info=expInfo(e))
    case col.MapMember(x, xs) => silver.MapContains(exp(x), exp(xs))(pos=pos(e), info=expInfo(e))

    case col.SetMinus(xs, ys) => silver.AnySetMinus(exp(xs), exp(ys))(pos=pos(e), info=expInfo(e))
    case col.BagMinus(xs, ys) => silver.AnySetMinus(exp(xs), exp(ys))(pos=pos(e), info=expInfo(e))
    case col.SetUnion(xs, ys) => silver.AnySetUnion(exp(xs), exp(ys))(pos=pos(e), info=expInfo(e))
    case col.BagAdd(xs, ys) => silver.AnySetUnion(exp(xs), exp(ys))(pos=pos(e), info=expInfo(e))
    case col.SetIntersection(xs, ys) => silver.AnySetIntersection(exp(xs), exp(ys))(pos=pos(e), info=expInfo(e))
    case col.BagLargestCommon(xs, ys) => silver.AnySetIntersection(exp(xs), exp(ys))(pos=pos(e), info=expInfo(e))

    case col.MapCons(m, k, v) => silver.MapUpdate(exp(m), exp(k), exp(v))(pos=pos(e), info=expInfo(e))
    case col.MapKeySet(m) => silver.MapDomain(exp(m))(pos=pos(e), info=expInfo(e))
    case col.MapValueSet(m) => silver.MapRange(exp(m))(pos=pos(e), info=expInfo(e))
    case col.MapItemSet(m) => ???
    case col.MapRemove(m, k) => ???
    case get @ col.MapGet(m, k) => currentMapGet.having(get) {
      silver.MapLookup(exp(m), exp(k))(pos=pos(e), info=expInfo(e))
    }

    case other => ??(other)
  }

  def trigger(patterns: Seq[col.Expr[_]]): silver.Trigger =
    silver.Trigger(patterns.map(exp))()

  def pred(p: col.PredicateApply[_]): silver.PredicateAccessPredicate =
    silver.PredicateAccessPredicate(silver.PredicateAccess(p.args.map(exp), ref(p.ref))(pos=pos(p), info=expInfo(p)), exp(p.perm))(pos=pos(p), info=expInfo(p))

  def predInTrigger(p: col.PredicateApply[_]): silver.PredicateAccess =
    silver.PredicateAccess(p.args.map(exp), ref(p.ref))(pos=pos(p), info=expInfo(p))

  def acc(e: col.Expr[_]): silver.LocationAccess = e match {
    case col.PredicateApply(Ref(pred), args, _) => silver.PredicateAccess(args.map(exp), ref(pred))(pos=pos(pred), info=expInfo(pred))
    case col.SilverDeref(obj, Ref(field)) => silver.FieldAccess(exp(obj), fields(field))(pos=pos(e), info=expInfo(e))
    case other => ??(other)
  }

  def stat(s: col.Statement[_]): silver.Stmt = s match {
    case inv@col.InvokeProcedure(method, args, outArgs, Nil, Nil, Nil) =>
      silver.MethodCall(ref(method), args.map(exp), outArgs.collect { case col.Local(Ref(arg)) => silver.LocalVar(ref(arg), typ(arg.t))()})(
        pos(s), NodeInfo(inv), silver.NoTrafos)
    case col.SilverFieldAssign(obj, field, value) =>
      silver.FieldAssign(silver.FieldAccess(exp(obj), fields(field.decl))(pos=pos(s), info=NodeInfo(s)), exp(value))(pos=pos(s), info=NodeInfo(s))
    case col.SilverLocalAssign(v, value) =>
      silver.LocalVarAssign(silver.LocalVar(ref(v), typ(v.decl.t))(pos=pos(s), info=NodeInfo(s)), exp(value))(pos=pos(s), info=NodeInfo(s))
    case col.Block(statements) => silver.Seqn(statements.map(stat), Seq())(pos=pos(s), info=NodeInfo(s))
    case col.Scope(locals, body) =>
      val silverLocals = locals.map(variable)
      silver.Seqn(Seq(stat(body)), silverLocals)(pos=pos(s), info=NodeInfo(s))
    case col.Branch(Seq((cond, whenTrue), (col.BooleanValue(true), whenFalse))) => silver.If(exp(cond), block(whenTrue), block(whenFalse))(pos=pos(s), info=NodeInfo(s))
    case col.Loop(col.Block(Nil), cond, col.Block(Nil), invNode @ col.LoopInvariant(inv, decrClause), body) =>
      silver.While(exp(cond), currentInvariant.having(invNode) { unfoldStar(inv).map(exp) ++ decrClause.map(decreases).toSeq }, block(body))(pos=pos(s), info=NodeInfo(s))
    case col.Label(decl, col.Block(Nil)) => silver.Label(ref(decl), Seq())(pos=pos(s), info=NodeInfo(s))
    case col.Goto(lbl) => silver.Goto(ref(lbl))(pos=pos(s), info=NodeInfo(s))
    case col.Return(col.Void()) => silver.Seqn(Nil, Nil)(pos=pos(s), info=NodeInfo(s))
    case col.Exhale(res) => silver.Exhale(exp(res))(pos=pos(s), info=NodeInfo(s))
    case col.Assert(assn) => silver.Assert(exp(assn))(pos=pos(s), info=NodeInfo(s))
    case col.Inhale(res) => silver.Inhale(exp(res))(pos=pos(s), info=NodeInfo(s))
    case col.Assume(assn) =>
      // PB: OK, since assn is type-checked boolean and hence equivalent.
      silver.Inhale(exp(assn))(pos=pos(s), info=NodeInfo(s))
    case col.Fold(p: col.PredicateApply[_]) => silver.Fold(pred(p))(pos=pos(s), info=NodeInfo(s))
    case col.Unfold(p: col.PredicateApply[_]) => silver.Unfold(pred(p))(pos=pos(s), info=NodeInfo(s))
    case col.SilverNewRef(v, fs) => silver.NewStmt(silver.LocalVar(ref(v), typ(v.decl.t))(), fs.map(ref => fields(ref.decl)))(pos=pos(s), info=NodeInfo(s))
    case col.WandPackage(wand @ col.Wand(left, right), proof) =>
      silver.Package(
        silver.MagicWand(exp(left), exp(right))(pos = pos(wand), info=NodeInfo(wand)), stat(proof) match {
          // ugly hack to fix scoping issue in viper
          case seqn: silver.Seqn => seqn
          case other => silver.Seqn(Seq(other), Nil)(pos = pos(proof), info=NodeInfo(proof))
        }
      )(pos = pos(s), info=NodeInfo(s))
    case col.WandApply(wand @ col.Wand(left, right)) =>
      silver.Apply(silver.MagicWand(exp(left), exp(right))(pos = pos(wand), info=NodeInfo(wand)))(pos = pos(s), info=NodeInfo(s))
    case other => ??(other)
  }

  def block(s: col.Statement[_]): silver.Seqn = stat(s) match {
    case seqn: silver.Seqn => seqn
    case other => silver.Seqn(Seq(other), Seq())(pos=pos(s), info=NodeInfo(s))
  }
}
