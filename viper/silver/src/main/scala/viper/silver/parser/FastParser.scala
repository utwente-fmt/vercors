package viper.silver.parser

/**
  * Created by sahil on 24.05.16.
  */


import java.io.File

import fastparse.all._
import fastparse.core.{Mutable, ParseCtx, Parser}
import fastparse.Implicits.{Repeater, Sequencer}
import fastparse.WhitespaceApi
import fastparse.parsers.Combinators.Rule
import fastparse.parsers.Terminals.Pass
import java.nio.file.{Files, Path}

import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import viper.silver.ast.SourcePosition
import viper.silver.FastPositions
import viper.silver.verifier.{ParseError, ParseReport, ParseWarning}

import scala.util.parsing.input.NoPosition


case class ParseException(msg: String, pos: scala.util.parsing.input.Position)  extends Exception



object FastParser extends PosParser{

  var _imports: mutable.HashMap[Path, Boolean] = null
  var _lines : Array[Int] = null


  def parse(s: String, f: Path) = {
    _file = f
    _imports = mutable.HashMap((f, true))
    val lines = s.linesWithSeparators
    _lines = lines.map(_.length).toArray

    try {
      val rp = RecParser(f).parses(s)
      rp match {
        case _ => rp
      }
    }
    catch {
      case e@ParseException(msg, pos) => {
        var line = 0
        var column = 0
        if (pos != null){
          line = pos.line
          column = pos.column
        }
        new ParseError(msg , SourcePosition(_file, line, column))
      }


    }


  }

  case class RecParser(file: Path) {
    def parses(s: String) = fastparser.parse(s)
  }

  val White = PWrapper {
    import fastparse.all._

    NoTrace((("/*" ~ (AnyChar ~ !StringIn("*/")).rep ~ AnyChar ~ "*/") | ("//" ~ CharsWhile(_ != '\n').? ~ ("\n" | End)) | " " | "\t" | "\n" | "\r").rep)
  }

  import fastparse.noApi._

  import White._


  // Actual Parser starts from here
  def identContinues = CharIn('0' to '9', 'A' to 'Z', 'a' to 'z', "$_")
  def keyword(check: String) = check ~~ !identContinues

  def parens[A](p: fastparse.noApi.Parser[A]) = "(" ~ p ~ ")"

  def quoted[A](p:fastparse.noApi.Parser[A]) = "\"" ~ p ~ "\""

  def foldPExp[E <: PExp](e: PExp, es: Seq[PExp => E]): E =
    es.foldLeft(e) { (t, a) =>
      val result = a(t)
      result.setPos(t)
      result
    }.asInstanceOf[E]

  def isFieldAccess(obj: Any) = {
    obj.isInstanceOf[PFieldAccess]
  }

  def expandDefines(defines: Seq[PDefine], toExpand: Seq[PDefine]): Seq[PDefine] = {
    val maxCount = 25
    /* TODO: Totally arbitrary cycle breaker. We should properly detect
     * (mutually) recursive named assertions.
     */
    var count = 0
    var definesToExpand = toExpand
    var expandedIds = Seq[String]()

    do {
      expandedIds = Seq.empty
      count += 1

      definesToExpand = definesToExpand.map(define => {
        val optExpandedDefine = doExpandDefines[PDefine](defines, define)
        expandedIds = optExpandedDefine.map(_.idndef.name).toSeq ++ expandedIds

        optExpandedDefine.getOrElse(define)
      })
    } while (expandedIds.nonEmpty && count <= maxCount)

    if (count > maxCount)
      sys.error(s"Couldn't expand all named assertions in $maxCount cycles. "
        + s"Might there be a mutual recursion involving $expandedIds?")

    definesToExpand
  }

  def doExpandDefines[N <: PNode](defines: Seq[PDefine], node: N): Option[N] = {
    var expanded = false

    def checkMacroType(oldNode: PNode, newNode: PNode): Unit = {
      if (oldNode.isInstanceOf[PStmt] && !newNode.isInstanceOf[PStmt]) {
        throw new ParseException("Expression macro used as statement", FastPositions.getStart(oldNode))
      }
      if (oldNode.isInstanceOf[PExp] && !newNode.isInstanceOf[PExp]) {
        throw new ParseException("Statement macro used as expression", FastPositions.getStart(oldNode))
      }
    }

    def lookupOrElse(piu: PIdnUse, els: PNode) =
      defines.find(_.idndef.name == piu.name).fold[PNode](els) _

    def expandAllegedInvocation(target: PIdnUse, targetArgs: Seq[PExp], els: PNode): PNode = {
      /* Potentially expand a named assertion that takes arguments, e.g. A(x, y) */
      lookupOrElse(target, els)(define => define.args match {
        case None =>
          /* There is a named assertion with name `target`, but the named
           * assertion takes arguments. Hence, `target` cannot denote the
           * use of a named assertion.
           */
          els
        case Some(args) if targetArgs.length != args.length =>
          throw new ParseException("Number of arguments does not match", FastPositions.getStart(target))
        case Some(args) =>
          expanded = true
          define.body.transform {
            /* Expand the named assertion's formal arguments by the given actual arguments */
            case piu: PIdnUse =>
              args.indexWhere(_.name == piu.name) match {
                case -1 => piu
                case i => targetArgs(i)
              }
          }(post = {
            case n => {
              FastPositions.setStart(n, target.start, true)
              FastPositions.setFinish(n, target.finish, true)
              n
            }
          }, resultCheck = {
            case (o,n) => checkMacroType(o, n)
          }) : PNode /* [2014-06-31 Malte] Type-checker wasn't pleased without it */
      })
    }
    val potentiallyExpandedNode =

      node.transform {
        case piu: PIdnUse =>
          /* Potentially expand a named assertion that takes no arguments, e.g. A.
           * If piu (which is a symbol) denotes a named assertion (i.e. if there
           * is a define in defines whose name is piu), then it is replaced by
           * the body of the named assertion.
           */
          lookupOrElse(piu, piu)(define => {
            expanded = true
            if(define.args.isDefined && !define.args.get.isEmpty) {
              throw new ParseException("Number of arguments does not match", FastPositions.getStart(piu))
            }
            define.body.transform()(post = {
              case n => {
                FastPositions.setStart(n, piu.start, true)
                FastPositions.setFinish(n, piu.finish, true)
                n
              }
            }, resultCheck = {
              case (o,n) => checkMacroType(o, n)
            })
          })

        case pmac: PMacroRef => pmac.idnuse match {
          case piu: PIdnUse =>
            /* Same as expanding named assertion in previous case for PIdnUse*/
            lookupOrElse(piu, pmac)(define => {
              expanded = true
              if(define.args.isDefined && !define.args.get.isEmpty) {
                throw new ParseException("Number of arguments does not match", FastPositions.getStart(piu))
              }
              define.body.transform()(post = {
                case n => {
                  FastPositions.setStart(n, piu.start, true)
                  FastPositions.setFinish(n, piu.finish, true)
                  n
                }
              }, resultCheck = {
                case (o,n) => checkMacroType(o, n)
              })
            })
        }

        case fapp: PCall => {
          expandAllegedInvocation(fapp.func, fapp.args, fapp)
        }
        case call: PMethodCall => {
          expandAllegedInvocation(call.method, call.args, call)
        }
      }(recursive = _ => true,
        resultCheck = {
          case (o,n) => checkMacroType(o, n)
        })

    if (expanded) Some(potentiallyExpandedNode)
    else None
  }

  /** The file we are currently parsing (for creating positions later). */
  def file: Path = _file

  def expandDefines[N <: PNode](defines: Seq[PDefine], node: N): N =
    doExpandDefines(defines, node).getOrElse(node)


  val keywords = Set("result",
    // types
    "Int", "Perm", "Bool", "Ref", "Rational",
    // boolean constants
    "true", "false",
    // null
    "null",
    // preamble importing
    "import",
    // declaration keywords
    "method", "function", "predicate", "program", "domain", "axiom", "var", "returns", "field", "define", "wand",
    // specifications
    "requires", "ensures", "invariant",
    // statements
    "fold", "unfold", "inhale", "exhale", "new", "assert", "assume", "package", "apply",
    // control flow
    "while", "if", "elseif", "else", "goto", "label",
    // special fresh block
    "fresh", "constraining",
    // sequences
    "Seq",
    // sets and multisets
    "Set", "Multiset", "union", "intersection", "setminus", "subset",
    // prover hint expressions
    "unfolding", "in", "folding", "applying", "packaging",
    // old expression
    "old", "lhs",
    // other expressions
    "let",
    // quantification
    "forall", "exists", "forperm",
    // permission syntax
    "acc", "wildcard", "write", "none", "epsilon", "perm",
    // modifiers
    "unique")


  lazy val atom: P[PExp] = P(integer | booltrue | boolfalse | nul | old | applyOld
    | result | unExp
    | "(" ~ exp ~ ")" | accessPred | inhaleExhale | perm | let | quant | forperm | unfolding | folding | applying
    | packaging | setTypedEmpty | explicitSetNonEmpty | explicitMultisetNonEmpty | multiSetTypedEmpty | seqTypedEmpty
    | seqLength | explicitSeqNonEmpty | seqRange | fapp | typedFapp | idnuse)


  lazy val result: P[PResultLit] = P(keyword("result").map { _ => PResultLit() })

  lazy val unExp: P[PUnExp] = P((CharIn("-!+").! ~ suffixExpr).map { case (a, b) => PUnExp(a, b) })

  lazy val integer: P[PIntLit] = P(CharIn('0' to '9').rep(1)).!.map { s => PIntLit(BigInt(s)) }

  lazy val booltrue: P[PBoolLit] = P(keyword("true")).map(_ => PBoolLit(b = true))

  lazy val boolfalse: P[PBoolLit] = P(keyword("false")).map(_ => PBoolLit(b = false))

  lazy val nul: P[PNullLit] = P(keyword("null")).map(_ => PNullLit())

  lazy val identifier: P[Unit] = P(CharIn('A' to 'Z', 'a' to 'z', "$_") ~~ CharIn('0' to '9', 'A' to 'Z', 'a' to 'z', "$_").repX)

  lazy val ident: P[String] = P(identifier.!).filter{case a => !keywords.contains(a)}.opaque("invalid identifier (could be a keyword)")

  lazy val idnuse: P[PIdnUse] = P(ident).map(PIdnUse)

  lazy val old: P[PExp] = P(StringIn("old") ~ (parens(exp).map(POld) | ("[" ~ idnuse ~ "]" ~ parens(exp)).map { case (a, b) => PLabelledOld(a, b) }))

  lazy val applyOld: P[PExp] = P((StringIn("lhs") ~ parens(exp)).map(PApplyOld))

  lazy val magicWandExp: P[PExp] = P(orExp ~ ("--*".! ~   exp ).?).map{ case(a, b) => b match {
      case Some(c) => PBinExp(a, c._1,c._2 )
      case None => a
    }
  }

  lazy val realMagicWandExp: P[PExp] = P((orExp ~ "--*".! ~ magicWandExp).map { case (a, b, c) => PBinExp(a, b, c) })

  lazy val implExp: P[PExp] = P(magicWandExp ~ (StringIn("==>").! ~ implExp).?).map{ case(a, b) => b match {
      case Some(c) => PBinExp(a, c._1,c._2 )
      case None => a
    }
  }
  lazy val iffExp: P[PExp] = P(implExp ~ ("<==>".! ~ iffExp).?).map{ case(a, b) => b match {
      case Some(c) => PBinExp(a, c._1,c._2 )
      case None => a
    }
  }
  lazy val iteExpr: P[PExp] = P(iffExp ~ ("?" ~ iteExpr ~ ":" ~ iteExpr).?).map{ case(a, b) => b match {
      case Some(c) => PCondExp(a, c._1,c._2 )
      case None => a
    }
  }
  lazy val exp: P[PExp] = P(iteExpr)

  lazy val suffix: fastparse.noApi.Parser[PExp => PExp] =
    P(("." ~ idnuse).map { id => (e: PExp) => PFieldAccess(e, id) } |
      ("[.." ~/ exp ~ "]").map { n => (e: PExp) => PSeqTake(e, n) } |
      ("[" ~ exp ~ "..]").map { n => (e: PExp) => PSeqDrop(e, n) } |
      ("[" ~ exp ~ ".." ~ exp ~ "]").map { case (n, m) => (e: PExp) => PSeqDrop(PSeqTake(e, m), n) } |
      ("[" ~ exp ~ "]").map { e1 => (e0: PExp) => PSeqIndex(e0, e1) } |
      ("[" ~ exp ~ ":=" ~ exp ~ "]").map { case (i, v) => (e: PExp) => PSeqUpdate(e, i, v) })

  lazy val suffixExpr: P[PExp] = P((atom ~ suffix.rep).map { case (fac, ss) => foldPExp[PExp](fac, ss) })

  lazy val realSuffixExpr: P[PExp] = P((atom ~ suffix.rep).map { case (fac, ss) => foldPExp[PExp](fac, ss) })

  lazy val termOp: P[String] = P(StringIn("*", "/", "\\", "%").!)

  lazy val term: P[PExp] = P((suffixExpr ~ termd.rep).map { case (a, ss) => foldPExp[PExp](a, ss) })

  lazy val termd: P[PExp => PBinExp] = P(termOp ~ suffixExpr).map { case (op, id) => (e: PExp) => PBinExp(e, op, id) }

  lazy val sumOp: P[String] = P(StringIn("++", "+", "-").! | keyword("union").! | keyword("intersection").! | keyword("setminus").! | keyword("subset").!)

  lazy val sum: P[PExp] = P((term ~ sumd.rep).map { case (a, ss) => foldPExp[PBinExp](a, ss) })

  lazy val sumd: P[PExp => PBinExp] = P(sumOp ~ term).map { case (op, id) => (e: PExp) => PBinExp(e, op, id) }

  lazy val cmpOp = P(StringIn("<=", ">=", "<", ">").! | keyword("in").!)

  lazy val cmpExp: P[PExp] = P(sum ~ (cmpOp ~ cmpExp).?).map{ case(a, b) => b match {
      case Some(c) => PBinExp(a, c._1,c._2 )
      case None => a
    }
  }

  lazy val eqOp = P(StringIn("==", "!=").!)

  lazy val eqExp: P[PExp] = P(cmpExp ~ (eqOp ~ eqExp).?).map { case (a, b) => b match {
        case Some(c) => PBinExp(a, c._1, c._2 )
        case None => a
      }
  }
  lazy val andExp: P[PExp] = P(eqExp ~ ("&&".! ~ andExp).?).map { case (a, b) => b match {
      case Some(c) => PBinExp(a, c._1, c._2 )
      case None => a
    }
  }
  lazy val orExp: P[PExp] = P(andExp ~ ("||".! ~ orExp).?).map { case (a, b) => b match {
      case Some(c) => PBinExp(a, c._1, c._2 )
      case None => a
    }
  }

  lazy val accessPredImpl: P[PAccPred] = P((keyword("acc") ~/ "(" ~ locAcc ~ ("," ~ exp).? ~ ")").map {
    case (loc, perms) => PAccPred(loc, perms.getOrElse(PFullPerm()))
  })

  lazy val accessPred: P[PAccPred] = P(accessPredImpl.map {
    case acc => {
      val perm = acc.perm
      if (FastPositions.getStart(perm) == NoPosition){
        FastPositions.setStart(perm, acc.start)
        FastPositions.setFinish(perm, acc.finish)
      }
      acc
    }
  })

  lazy val locAcc: P[PLocationAccess] = P(fieldAcc | predAcc)

  lazy val fieldAcc: P[PFieldAccess] = P(realSuffixExpr.filter(isFieldAccess).map { case fa: PFieldAccess => fa case _ => sys.error("not a field access")})

  lazy val predAcc: P[PLocationAccess] = P(fapp)

  lazy val actualArgList: P[Seq[PExp]] = P(exp.rep(sep = ","))

  lazy val inhaleExhale: P[PExp] = P("[" ~ exp ~ "," ~ exp ~ "]").map { case (a, b) => PInhaleExhaleExp(a, b) }

  lazy val perm: P[PExp] = P(keyword("none").map(_ => PNoPerm()) | keyword("wildcard").map(_ => PWildcard()) | keyword("write").map(_ => PFullPerm())
    | keyword("epsilon").map(_ => PEpsilon()) | ("perm" ~ parens(locAcc)).map(PCurPerm))

  lazy val let: P[PExp] = P(
    ("let" ~/ idndef ~ "==" ~ "(" ~ exp ~ ")" ~ "in" ~ exp).map { case (id, exp1, exp2) =>
      /* Type unresolvedType is expected to be replaced with the type of exp1
       * after the latter has been resolved
       * */
      val unresolvedType = PUnknown().setPos(id)
      val formalArgDecl = PFormalArgDecl(id, unresolvedType).setPos(id)
      val nestedScope = PLetNestedScope(formalArgDecl, exp2).setPos(exp2)

      PLet(exp1, nestedScope)
    })

  lazy val idndef: P[PIdnDef] = P(ident).map(PIdnDef)

  lazy val quant: P[PExp] = P((keyword("forall") ~/ nonEmptyFormalArgList ~ "::" ~/ trigger.rep ~ exp).map { case (a, b, c) => PForall(a, b, c) } |
    (keyword("exists") ~/ nonEmptyFormalArgList ~ "::" ~ exp).map { case (a, b) => PExists(a, b) })

  lazy val nonEmptyFormalArgList: P[Seq[PFormalArgDecl]] = P(formalArg.rep(min = 1, sep = ","))

  lazy val formalArg: P[PFormalArgDecl] = P(idndef ~ ":" ~ typ).map { case (a, b) => PFormalArgDecl(a, b) }

  lazy val typ: P[PType] = P(primitiveTyp | domainTyp | seqType | setType | multisetType)

  lazy val domainTyp: P[PDomainType] = P((idnuse ~ "[" ~ typ.rep(sep = ",") ~ "]").map { case (a, b) => PDomainType(a, b) } |
    idnuse.map {
      // domain type without type arguments (might also be a type variable)
      case name => PDomainType(name, Nil)
    })

  lazy val seqType: P[PType] = P(keyword("Seq") ~/ "[" ~ typ ~ "]").map(PSeqType)

  lazy val setType: P[PType] = P(keyword("Set") ~/ "[" ~ typ ~ "]").map(PSetType)

  lazy val multisetType: P[PType] = P(keyword("Multiset") ~/ "[" ~ typ ~ "]").map(PMultisetType)

  lazy val primitiveTyp: P[PType] = P(keyword("Rational").map { case _ => PPrimitiv("Perm") }
                                      | (StringIn("Int", "Bool", "Perm", "Ref") ~~ !identContinues).!.map(PPrimitiv))

  lazy val trigger: P[Seq[PExp]] = P("{" ~/ exp.rep(sep = ",", min = 1) ~ "}")

  lazy val forperm: P[PExp] = P(keyword("forperm") ~ "[" ~ idnuse.rep(sep = ",") ~ "]" ~ idndef ~ "::" ~/ exp).map {
    case (ids, id, body) => PForPerm(PFormalArgDecl(id, PPrimitiv("Ref")), ids, body)
  }

  lazy val unfolding: P[PExp] = P(keyword("unfolding") ~/ predicateAccessPred ~ "in" ~ exp).map { case (a, b) => PUnfolding(a, b) }

  lazy val predicateAccessPred: P[PAccPred] = P(accessPred | predAcc.map {
    case loc => {
      val perm = PFullPerm()
      FastPositions.setStart(perm, loc.start)
      FastPositions.setFinish(perm, loc.finish)
      PAccPred(loc, perm)
    }
  })

  lazy val folding: P[PExp] = P(keyword("folding") ~/ predicateAccessPred ~ "in" ~ exp).map { case (a, b) => PFoldingGhostOp(a, b) }

  lazy val applying: P[PExp] =
  /**
   * We must be careful here to not create ambiguities in our grammar.
   * when 'magicWandExp' is used instead of the more specific
   * 'realMagicWandExp | idnuse', then the following problem can occur:
   * Consider an expression such as "applying w in A". The parser
   * will interpret "w in A" as a set-contains expression, which is
   * fine according to our rules.
   * The outer applying-rule will fail.
   * Possible solution is that we should backtrack enough
   * to reparse "w in A", but this time as desired, not as a
   * set-contains expression.
   */

  P("applying" ~ ("(" ~ realMagicWandExp ~ ")" | idnuse) ~ ("in" ~ exp)).map { case (a, b) => PApplyingGhostOp(a, b) }


  lazy val packaging: P[PExp] = /* See comment on applying */
    P("packaging" ~ ("(" ~ realMagicWandExp ~ ")" | idnuse) ~ "in" ~ exp).map { case (a, b) => PPackagingGhostOp(a, b) }

  lazy val setTypedEmpty: P[PExp] = P("Set" ~ "[" ~ typ ~ "]" ~ "(" ~ ")").map { case a => PEmptySet(a) }

  lazy val explicitSetNonEmpty: P[PExp] = P("Set"  ~ "(" ~/ exp.rep(sep = ",", min = 1) ~ ")").map(PExplicitSet)

  lazy val explicitMultisetNonEmpty: P[PExp] = P("Multiset" ~ "(" ~/ exp.rep(min = 1, sep = ",") ~ ")").map { case elems => PExplicitMultiset(elems) }

  lazy val multiSetTypedEmpty: P[PExp] = P("Multiset" ~ "[" ~/ typ ~ "]" ~ "(" ~ ")").map(PEmptyMultiset)

  lazy val seqTypedEmpty: P[PExp] = P("Seq[" ~ typ ~ "]()").map(PEmptySeq)

  lazy val seqLength: P[PExp] = P("|" ~ exp ~ "|").map(PSize)

  lazy val explicitSeqNonEmpty: P[PExp] = P("Seq(" ~/ exp.rep(min = 1, sep = ",") ~ ")").map {
    //      case Nil => PEmptySeq(PUnknown())
    case elems => PExplicitSeq(elems)
  }

  lazy val seqRange: P[PExp] = P("[" ~ exp ~ ".." ~ exp ~ ")").map { case (a, b) => { PRangeSeq(a, b) } }

  lazy val fapp: P[PCall] = P(idnuse ~ parens(actualArgList)).map {
    case (func, args) => PCall(func, args, None)
  }

  lazy val typedFapp: P[PExp] = P(parens(idnuse ~ parens(actualArgList) ~ ":" ~ typ)).map {
    case (func, args, typeGiven) => PCall(func, args, Some(typeGiven))
  }



  lazy val stmt: P[PStmt] = P(fieldassign | localassign | fold | unfold | exhale | assertP |
    inhale | ifthnels | whle | varDecl | defineDecl | letwandDecl | newstmt | fresh | constrainingBlock |
    methodCall | goto | lbl | packageWand | applyWand| macroref)

  lazy val nodefinestmt: P[PStmt] = P(fieldassign | localassign | fold | unfold | exhale | assertP |
    inhale | ifthnels | whle | varDecl  | letwandDecl | newstmt | fresh | constrainingBlock |
    methodCall | goto | lbl | packageWand | applyWand | macroref)

  lazy val macroref: P[PMacroRef] = P(idnuse).map{ case(a) => PMacroRef(a)}

  lazy val fieldassign: P[PFieldAssign] = P(fieldAcc ~ ":=" ~ exp).map { case (a, b) => PFieldAssign(a, b) }

  lazy val localassign: P[PVarAssign] = P(idnuse ~ ":=" ~ exp).map { case (a, b) => PVarAssign(a, b) }

  lazy val fold: P[PFold] = P("fold" ~ predicateAccessPred).map(PFold)

  lazy val unfold: P[PUnfold] = P("unfold" ~ predicateAccessPred).map(PUnfold)

  lazy val exhale: P[PExhale] = P(keyword("exhale") ~/ exp).map(PExhale)

  lazy val assertP: P[PAssert] = P(keyword("assert") ~/ exp).map(PAssert)

  lazy val inhale: P[PInhale] = P((keyword("inhale") | keyword("assume")) ~ exp).map(PInhale)

  lazy val ifthnels: P[PIf] = P("if" ~ "(" ~ exp ~ ")" ~ block ~ elsifEls).map {
    case (cond, thn, ele) => PIf(cond, PSeqn(thn), ele)
  }

  lazy val block: P[Seq[PStmt]] = P("{" ~ stmts ~ "}")

  lazy val stmts: P[Seq[PStmt]] = P(stmt ~/ ";".?).rep

  lazy val elsifEls: P[PStmt] = P(elsif | els)

  lazy val elsif: P[PStmt] = P("elseif" ~/ "(" ~ exp ~ ")" ~ block ~ elsifEls).map {
    case (cond, thn, ele) => PIf(cond, PSeqn(thn), ele)
  }

  lazy val els: P[PStmt] = (keyword("else") ~/ block).?.map { block => PSeqn(block.getOrElse(Nil)) }

  lazy val whle: P[PWhile] = P(keyword("while") ~/ "(" ~ exp ~ ")" ~ inv.rep ~ block).map {
    case (cond, invs, body) => PWhile(cond, invs, PSeqn(body))
  }

  lazy val inv: P[PExp] = P(keyword("invariant") ~ exp ~ ";".?)

  lazy val varDecl: P[PLocalVarDecl] = P(keyword("var") ~/ idndef ~ ":" ~ typ ~ (":=" ~ exp).?).map { case (a, b, c) => PLocalVarDecl(a, b, c) }

  lazy val defineDecl: P[PDefine] = P(keyword("define") ~/ idndef ~ ("(" ~ idndef.rep(sep = ",") ~ ")").? ~ (exp|"{"~ (nodefinestmt ~ ";".?).rep ~ "}")).map {
    case (a, b, c) => c match {
      case e: PExp => PDefine(a,b,e)
      case ss: Seq[PStmt] @unchecked => PDefine(a,b,PSeqn(ss))
    }
  }

  lazy val letwandDecl: P[PLetWand] = P(keyword("wand") ~/ idndef ~ ":=" ~ exp).map { case (a, b) => PLetWand(a, b) }

  lazy val newstmt: P[PNewStmt] = P(idnuse ~ ":=" ~ "new" ~ "(" ~ starOrFields ~ ")").map { case (a, b) => PNewStmt(a, b) }

  lazy val starOrFields: P[Option[Seq[PIdnUse]]] = P(("*").!.map { _ => None } | (idnuse.rep(sep = ",").map { fields => Some(fields) }))

  lazy val fresh: P[PFresh] = P(keyword("fresh") ~ idnuse.rep(sep = ",")).map { case vars => PFresh(vars) }

  lazy val constrainingBlock: P[PConstraining] = P("constraining" ~ "(" ~ idnuse.rep(sep = ",") ~ ")" ~ block).map { case (vars, s) => PConstraining(vars, PSeqn(s)) }

  lazy val methodCall: P[PMethodCall] = P((idnuse.rep(sep = ",") ~ ":=").? ~ idnuse ~ parens(exp.rep(sep = ","))).map {
    case (None, method, args) => PMethodCall(Nil, method, args)
    case (Some(targets), method, args) => PMethodCall(targets, method, args)
  }

  lazy val goto: P[PGoto] = P("goto" ~/ idnuse).map(PGoto)

  lazy val lbl: P[PLabel] = P(keyword("label") ~/ idndef ~ (keyword("invariant") ~/ exp).rep ).map{ case (name, invs) => PLabel(name, invs)}

  lazy val packageWand: P[PPackageWand] = P("package" ~/ magicWandExp).map(PPackageWand)

  lazy val applyWand: P[PApplyWand] = P("apply" ~/ magicWandExp).map(PApplyWand)

  lazy val programDecl: P[PProgram] = P((preambleImport | defineDecl | domainDecl | fieldDecl | functionDecl | predicateDecl | methodDecl).rep).map {
    case decls =>
      var globalDefines: Seq[PDefine] = decls.collect { case d: PDefine => d }
      globalDefines = expandDefines(globalDefines, globalDefines)

      val imports: Seq[PImport] = decls.collect { case i: PImport => i }

      val dups: Iterable[viper.silver.verifier.ParseError] = imports.groupBy(identity).collect {
        case (imp@PImport(x), List(_, _, _*)) =>
          val dup_pos = imp.start.asInstanceOf[viper.silver.ast.Position]
          val report = s"""multiple imports of the same file "$x" detected"""
          viper.silver.verifier.ParseError(report, dup_pos)
      }

      val imp_progs_results: Seq[Either[ParseReport, Any] with Product with Serializable] = imports.collect {
        case imp@PImport(imp_file) =>
          val imp_path = file.getParent.resolve(imp_file)
          val imp_pos = imp.start.asInstanceOf[viper.silver.ast.Position]

          if (java.nio.file.Files.notExists(imp_path))
            Left(viper.silver.verifier.ParseError(s"""file "$imp_path" does not exist""", imp_pos))

          else if (java.nio.file.Files.isSameFile(imp_path, file))
            Left(viper.silver.verifier.ParseError(s"""importing yourself is probably not a good idea!""", imp_pos))

          else if (_imports.put(imp_path, true).isEmpty) {
            val source = scala.io.Source.fromInputStream(Files.newInputStream(imp_path))
            val buffer = try {
              Right(source.getLines.toArray)
            } catch {
              case e@(_: RuntimeException | _: java.io.IOException) =>
                Left(viper.silver.verifier.ParseError(s"""could not import file ($e)""", imp_pos))
            } finally {
              source.close()
            }
            buffer match {
              case Left(e) => Left(e)
              case Right(s) =>
                //TODO print debug info iff --dbg switch is used
                val imported_source = s.mkString("\n") + "\n"
                val p = RecParser(imp_path).parses(imported_source)
                p match {
                  case fastparse.core.Parsed.Success(a, _) => Right(a)
                  case fastparse.core.Parsed.Failure(msg, next, extra) => Left(viper.silver.verifier.ParseError(s"Failure: $msg", FilePosition(imp_path, extra.line, extra.col)))
                }
            }
          }

          else {
            val report = s"found loop dependency among these imports:\n" +
              _imports.map { case (k, v) => k }.mkString("\n")
            println(s"warning: $report\n(loop starts at $imp_pos)")
            Right(ParseWarning(report, imp_pos))
          }
      }

      val imp_progs = imp_progs_results.collect { case Right(p) => p }

      val imp_reports = imp_progs_results.collect { case Left(e) => e } ++
        imp_progs.collect { case PProgram(_, _, _, _, _, _, e: List[ParseReport]) => e }.flatten ++
        dups

      val files =
        imp_progs.collect { case PProgram(f: Seq[PImport], _, _, _, _, _, _) => f }.flatten ++
          imports

      val domains =
        imp_progs.collect { case PProgram(_, d: Seq[PDomain], _, _, _, _, _) => d }.flatten ++
          decls.collect { case d: PDomain => expandDefines(globalDefines, d) }

      val fields =
        imp_progs.collect { case PProgram(_, _, f: Seq[PField], _, _, _, _) => f }.flatten ++
          decls.collect { case f: PField => f }

      val functions =
        imp_progs.collect { case PProgram(_, _, _, f: Seq[PFunction], _, _, _) => f }.flatten ++
          decls.collect { case d: PFunction => expandDefines(globalDefines, d) }

      val predicates =
        imp_progs.collect { case PProgram(_, _, _, _, p: Seq[PPredicate], _, _) => p }.flatten ++
          decls.collect { case d: PPredicate => expandDefines(globalDefines, d) }

      val methods =
        imp_progs.collect { case PProgram(_, _, _, _, _, m: Seq[PMethod], _) => m }.flatten ++
          decls.collect {
            case meth: PMethod =>
              var localDefines = meth.deepCollect { case n: PDefine => n }
              localDefines = expandDefines(localDefines ++ globalDefines, localDefines)

              val methWithoutDefines =
                if (localDefines.isEmpty)
                  meth
                else
                  meth.transform { case la: PDefine => PSkip().setPos(la) }()

              expandDefines(localDefines ++ globalDefines, methWithoutDefines)
          }

      PProgram(files, domains, fields, functions, predicates, methods, imp_reports)
  }

  lazy val preambleImport: P[PImport] = P(keyword("import") ~/ quoted(relativeFilePath.!)).map {
    case filename => PImport(filename)
  }

  lazy val relativeFilePath: P[String] = P((CharIn("~.").?).! ~~ (CharIn("/").? ~~ CharIn(".", 'A' to 'Z', 'a' to 'z', '0' to '9', "_- \n\t")).rep(1))

  lazy val domainDecl: P[PDomain] = P("domain" ~/ idndef ~ ("[" ~ domainTypeVarDecl.rep(sep = ",") ~ "]").? ~ "{" ~ (domainFunctionDecl | axiomDecl).rep ~
    "}").map {
    case (name, typparams, members) =>
      val funcs = members collect { case m: PDomainFunction1 => m}
      val axioms = members collect { case m: PAxiom1 => m}
      PDomain(
        name,
        typparams.getOrElse(Nil),
        funcs map (f => PDomainFunction(f.idndef, f.formalArgs, f.typ, f.unique)(PIdnUse(name.name)).setPos(f)),
        axioms map (a => PAxiom(a.idndef, a.exp)(PIdnUse(name.name)).setPos(a)))
  }

  lazy val domainTypeVarDecl: P[PTypeVarDecl] = P(idndef).map(PTypeVarDecl)

  lazy val domainFunctionDecl: P[PDomainFunction1] = P("unique".!.? ~ functionSignature ~ ";".?).map {
    case (unique, fdecl) => fdecl match {
      case (name, formalArgs, t) => PDomainFunction1(name, formalArgs, t, unique.isDefined)
    }
  }

  lazy val functionSignature = P("function" ~ idndef ~ "(" ~ formalArgList ~ ")" ~ ":" ~ typ)

  lazy val formalArgList: P[Seq[PFormalArgDecl]] = P( formalArg.rep(sep = ","))

  lazy val axiomDecl: P[PAxiom1] = P(keyword("axiom") ~ idndef ~ "{" ~ exp ~ "}" ~ ";".?).map { case (a, b) => PAxiom1(a, b) }

  lazy val fieldDecl: P[PField] = P("field" ~/ idndef ~ ":" ~ typ ~ ";".?).map { case (a, b) => PField(a, b) }

  lazy val functionDecl: P[PFunction] = P("function" ~/ idndef ~ "(" ~ formalArgList ~ ")" ~ ":" ~ typ ~ pre.rep ~
    post.rep ~ ("{" ~ exp ~ "}").?).map { case (a, b, c, d, e, f) => PFunction(a, b, c, d, e, f) }


  lazy val pre: P[PExp] = P("requires" ~/ exp ~ ";".?)

  lazy val post: P[PExp] = P("ensures" ~/ exp ~ ";".?)

  lazy val predicateDecl: P[PPredicate] = P("predicate" ~/ idndef ~ "(" ~ formalArgList ~ ")" ~ ("{" ~ exp ~ "}").?).map { case (a, b, c) => PPredicate(a, b, c) }

  lazy val methodDecl: P[PMethod] = P(methodSignature ~/ pre.rep ~ post.rep ~ block.?).map {
    case (name, args, rets, pres, posts, Some(body)) =>
      PMethod(name, args, rets.getOrElse(Nil), pres, posts, PSeqn(body))
    case (name, args, rets, pres, posts, None) =>
      PMethod(name, args, rets.getOrElse(Nil), pres, posts, PSeqn(Seq(PInhale(PBoolLit(b = false)))))
  }

  lazy val methodSignature = P("method" ~/ idndef ~ "(" ~ formalArgList ~ ")" ~ ("returns" ~ "(" ~ formalArgList ~ ")").?)

  lazy val fastparser: P[PProgram] = P( Start ~ programDecl ~ End)



}
