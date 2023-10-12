package vct.rewrite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.result.VerificationError.UserError
import vct.rewrite.SmtlibToProverTypes.NotRepresentable

import scala.collection.mutable
import scala.jdk.StreamConverters.IntStreamHasToScala

case object SmtlibToProverTypes extends RewriterBuilder {
  override def key: String = "smtlib"
  override def desc: String = "Encode smtlib types and functions into their stringified counterpart"

  case class NotRepresentable(t: Type[_]) extends UserError {
    override def code: String = "notSmtRepresentable"
    override def text: String = t.o.messageInContext("This type does not have a stable representation on the smt level yet.")
  }
}

case class SmtlibToProverTypes[Pre <: Generation]() extends Rewriter[Pre] {
  def smtTypeString(t: Type[Pre]): String = t match {
    case TSeq(element) => "$Seq<" + smtTypeString(element) + ">"
    case TSet(element) => "$Set<" + smtTypeString(element) + ">"
    case TBag(element) => "$Multiset<" + smtTypeString(element) + ">"
    case TMap(key, value) => "$Map<" + smtTypeString(key) + "~_" + smtTypeString(value) + ">"
    case TBool() => "Bool"
    case TRef() => "$Ref"
    case TInt() => "Int"
    case TRational() => "$Perm"
    case TAxiomatic(adt, args) => adt.decl.o.preferredName + "<" + args.map(smtTypeString).mkString("~_") + ">"
    case TProverType(ref) => ref.decl.interpretation.collectFirst { case (SmtLib(), int) => int }.get
    case TSmtlibArray(index, value) => s"(Array ${index.map(smtTypeString).mkString(" ")} ${smtTypeString(value)})"
    case TSmtlibBitVector(size) => s"(_ BitVec $size)"
    case TSmtlibRoundingMode() => "RoundingMode"
    case TSmtlibFloatingPoint(exponentBits, mantissaAndSignBits) => s"(_ FloatingPoint $exponentBits $mantissaAndSignBits)"
    case TSmtlibString() => "String"
    case TSmtlibRegLan() => "RegLan"
    case TSmtlibSeq(element) => s"(Seq ${smtTypeString(element)})"
    case other => throw NotRepresentable(other)
  }

  val declaredType: mutable.Map[String, ProverType[Post]] = mutable.Map()
  val declaredFunc: mutable.Map[(String, Seq[Type[Pre]]), ProverFunction[Post]] = mutable.Map()

  def getType(t: Type[Pre]): TProverType[Post] = {
    val asText = smtTypeString(t)
    TProverType(declaredType.getOrElseUpdate(asText, {
      implicit val o: Origin = t.o
      globalDeclarations.declare(new ProverType[Post](Seq(SmtLib[Post]() -> asText)))
    }).ref)
  }

  def getExpr(e: Expr[Pre], f: String, args: Expr[Pre]*)(implicit o: Origin): Expr[Post] =
    ProverFunctionInvocation(declaredFunc.getOrElseUpdate((f, args.map(_.t)), {
      globalDeclarations.declare(new ProverFunction[Post](
        Seq(SmtLib[Post]() -> f),
        args.map(arg => new Variable(dispatch(arg.t))),
        dispatch(e.t),
      ))
    }).ref, args.map(dispatch))

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case t: SmtlibType[Pre] => getType(t)
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case e: SmtlibExpr[Pre] =>
      implicit val o: Origin = e.o
      e match {
        case SmtlibSelect(arr, is) => getExpr(e, "select", (arr +: is): _*)
        case SmtlibStore(arr, is, x) => getExpr(e, "store", (arr +: is) :+ x: _*)
        case SmtlibBitvecLiteral(data) => getExpr(e, data.toSmt)
        case SmtlibConcat(left, right) => getExpr(e, "concat", left, right)
        case SmtlibExtract(inclusiveEndIndexFromRight, startIndexFromRight, bv) =>
          getExpr(e, s"(_ extract $inclusiveEndIndexFromRight $startIndexFromRight)", bv)
        case SmtlibBvNot(bv) => getExpr(e, "bvnot", bv)
        case SmtlibBvAnd(left, right) => getExpr(e, "bvand", left, right)
        case SmtlibBvOr(left, right) => getExpr(e, "bvor", left, right)
        case SmtlibBvNeg(bv) => getExpr(e, "bvneg", bv)
        case SmtlibBvAdd(left, right) => getExpr(e, "bvadd", left, right)
        case SmtlibBvMul(left, right) => getExpr(e, "bvmul", left, right)
        case SmtlibBvUDiv(left, right) => getExpr(e, "bvudiv", left, right)
        case SmtlibBvURem(left, right) => getExpr(e, "bvurem", left, right)
        case SmtlibBvShl(left, right) => getExpr(e, "bvshl", left, right)
        case SmtlibBvShr(left, right) => getExpr(e, "bvshr", left, right)
        case SmtlibBvULt(left, right) => getExpr(e, "bvult", left, right)
        case SmtlibRNE() => getExpr(e, "RNE")
        case SmtlibRNA() => getExpr(e, "RNA")
        case SmtlibRTP() => getExpr(e, "RTP")
        case SmtlibRTN() => getExpr(e, "RTN")
        case SmtlibRTZ() => getExpr(e, "RTZ")
        case SmtlibFp(sign, exponent, mantissa) => getExpr(e, "fp", sign, exponent, mantissa)
        case SmtlibFpAbs(arg) => getExpr(e, "fp.abs", arg)
        case SmtlibFpNeg(arg) => getExpr(e, "fp.neg", arg)
        case SmtlibFpAdd(left, right) => getExpr(e, "fp.add", left, right)
        case SmtlibFpSub(left, right) => getExpr(e, "fp.sub", left, right)
        case SmtlibFpMul(left, right) => getExpr(e, "fp.mul", left, right)
        case SmtlibFpDiv(left, right) => getExpr(e, "fp.div", left, right)
        case SmtlibFpFma(left, right, addend) => getExpr(e, "fp.fma", left, right, addend)
        case SmtlibFpSqrt(arg) => getExpr(e, "fp.sqrt", arg)
        case SmtlibFpRem(left, right) => getExpr(e, "fp.rem", left, right)
        case SmtlibFpRoundToIntegral(arg) => getExpr(e, "fp.roundToIntegral", arg)
        case SmtlibFpMin(left, right) => getExpr(e, "fp.min", left, right)
        case SmtlibFpMax(left, right) => getExpr(e, "fp.max", left, right)
        case SmtlibFpLeq(left, right) => getExpr(e, "fp.leq", left, right)
        case SmtlibFpLt(left, right) => getExpr(e, "fp.lt", left, right)
        case SmtlibFpGeq(left, right) => getExpr(e, "fp.geq", left, right)
        case SmtlibFpGt(left, right) => getExpr(e, "fp.gt", left, right)
        case SmtlibFpEq(left, right) => getExpr(e, "fp.eq", left, right)
        case SmtlibFpIsNormal(arg) => getExpr(e, "fp.isNormal", arg)
        case SmtlibFpIsSubnormal(arg) => getExpr(e, "fp.isSubnormal", arg)
        case SmtlibFpIsZero(arg) => getExpr(e, "fp.isZero", arg)
        case SmtlibFpIsInfinite(arg) => getExpr(e, "fp.isInfinite", arg)
        case SmtlibFpIsNaN(arg) => getExpr(e, "fp.isNaN", arg)
        case SmtlibFpIsNegative(arg) => getExpr(e, "fp.isNegative", arg)
        case SmtlibFpIsPositive(arg) => getExpr(e, "fp.isPositive", arg)
        case SmtlibToFp(bv, exponentBits, mantissaAndSignBits) =>
          getExpr(e, s"(_ to_fp $exponentBits $mantissaAndSignBits)", bv)
        case SmtlibFpCast(arg, exponentBits, mantissaAndSignBits) =>
          getExpr(e, s"(_ to_fp $exponentBits $mantissaAndSignBits)", arg)
        case SmtlibFpFromReal(arg, exponentBits, mantissaAndSignBits) =>
          getExpr(e, s"(_ to_fp $exponentBits $mantissaAndSignBits)", arg)
        case SmtlibFpFromSInt(bv, exponentBits, mantissaAndSignBits) =>
          getExpr(e, s"(_ to_fp $exponentBits $mantissaAndSignBits)", bv)
        case SmtlibFpFromUInt(bv, exponentBits, mantissaAndSignBits) =>
          getExpr(e, s"(_ to_fp_unsigned $exponentBits $mantissaAndSignBits)", bv)
        case SmtlibFpToReal(arg) => getExpr(e, "fp.to_real", arg)
        case SmtlibFpToSInt(arg, bits) => getExpr(e, s"(_ fp.to_sbv $bits)", arg)
        case SmtlibFpToUInt(arg, bits) => getExpr(e, s"(_ fp.to_ubv $bits)", arg)
        case SmtlibLiteralString(data) =>
          // PB: it seems "\" need not be escaped, except when an *entire* escape sequence occurs in the string
          //     literally in the string to be escaped. Here we just escape all backslashes, which is one way of
          //     ensuring we break up any accidental literal escape sequences :)
          val content = data.codePoints().toScala(Seq).map {
            case c if 0x20 <= c && c <= 0x7e && c != 0x5c => Character.toString(c)
            case c => "\\u{" + c.toHexString + "}"
          }
          getExpr(e, "\"" + content + "\"")
        case SmtlibStrConcat(left, right) => getExpr(e, "str.++", left, right)
        case SmtlibStrLen(arg) => getExpr(e, "str.len", arg)
        case SmtlibStrLt(left, right) => getExpr(e, "str.<", left, right)
        case SmtlibStrLeq(left, right) => getExpr(e, "str.<=", left, right)
        case SmtlibStrAt(str, i) => getExpr(e, "str.at", str, i)
        case SmtlibSubstr(str, i, n) => getExpr(e, "str.substr", str, i, n)
        case SmtlibStrPrefixOf(left, right) => getExpr(e, "str.prefixof", left, right)
        case SmtlibStrSuffixOf(left, right) => getExpr(e, "str.suffixof", left, right)
        case SmtlibStrContains(left, right) => getExpr(e, "str.contains", left, right)
        case SmtlibStrIndexOf(haystack, needle, fromIndex) => getExpr(e, "str.indexof", haystack, needle, fromIndex)
        case SmtlibStrReplace(haystack, needle, replacement) => getExpr(e, "str.replace", haystack, needle, replacement)
        case SmtlibStrReplaceAll(haystack, needle, replacement) => getExpr(e, "str.replace_all", haystack, needle, replacement)
        case SmtlibStrReplaceRe(haystack, re, replacement) => getExpr(e, "str.replace_re", haystack, re, replacement)
        case SmtlibStrReplaceReAll(haystack, re, replacement) => getExpr(e, "str.replace_re_all", haystack, re, replacement)
        case SmtlibStrIsDigit(arg) => getExpr(e, "str.indexof", arg)
        case SmtlibStrToCode(arg) => getExpr(e, "str.to_code", arg)
        case SmtlibStrFromCode(arg) => getExpr(e, "str.from_code", arg)
        case SmtlibStrToInt(arg) => getExpr(e, "str.to_int", arg)
        case SmtlibStrFromInt(arg) => getExpr(e, "str.from_int", arg)
        case SmtlibReFromStr(arg) => getExpr(e, "str.to_re", arg)
        case SmtlibReContains(re, str) => getExpr(e, "str.in_re", str, re)
        case SmtlibReNone() => getExpr(e, "re.none")
        case SmtlibReAll() => getExpr(e, "re.all")
        case SmtlibReAllChar() => getExpr(e, "re.allchar")
        case SmtlibReConcat(left, right) => getExpr(e, "re.++", left, right)
        case SmtlibReUnion(left, right) => getExpr(e, "re.union", left, right)
        case SmtlibReInter(left, right) => getExpr(e, "re.inter", left, right)
        case SmtlibReStar(arg) => getExpr(e, "re.*", arg)
        case SmtlibReComp(arg) => getExpr(e, "re.comp", arg)
        case SmtlibReDiff(left, right) => getExpr(e, "re.diff", left, right)
        case SmtlibRePlus(arg) => getExpr(e, "re.+", arg)
        case SmtlibReOpt(arg) => getExpr(e, "re.opt", arg)
        case SmtlibReRange(left, right) => getExpr(e, "re.range", left, right)
        case SmtlibReRepeat(count, arg) => getExpr(e, s"(_ re.^ $count)", arg)
        case SmtlibReRepeatRange(from, to, arg) => getExpr(e, s"(_ re.loop $from $to)", arg)
        case Z3BvSub(left, right) => getExpr(e, "bvsub", left, right)
        case Z3BvSRem(left, right) => getExpr(e, "bvsrem", left, right)
        case Z3BvSMod(left, right) => getExpr(e, "bvsmod", left, right)
        case Z3BvSShr(left, right) => getExpr(e, "bvashr", left, right)
        case Z3BvNand(left, right) => getExpr(e, "bvnand", left, right)
        case Z3BvNor(left, right) => getExpr(e, "bvnor", left, right)
        case Z3BvXnor(left, right) => getExpr(e, "bvxnor", left, right)
        case Z3ArrayConst(domain, codomain, value) =>
          getExpr(e, s"(as const ${smtTypeString(TSmtlibArray(domain, codomain))})", value)
        case Z3ArrayOfFunction(ref) =>
          // https://github.com/utwente-fmt/vercors/issues/1022
          getExpr(e, s"(_ as-array ${ref.ref.decl.o.preferredName})")
        case Z3ArrayMap(ref, args) =>
          getExpr(e, s"(_ map ${ref.ref.decl.o.preferredName})", args: _*)
        case Z3SeqEmpty(elementType) => getExpr(e, s"(as seq.empty (Seq ${smtTypeString(elementType)}))")
        case Z3SeqUnit(arg) => getExpr(e, "seq.unit", arg)
        case Z3SeqConcat(left, right) => getExpr(e, "seq.++", left, right)
        case Z3SeqLen(arg) => getExpr(e, "seq.len", arg)
        case Z3SeqExtract(seq, offset, len) => getExpr(e, "seq.extract", seq, offset, len)
        case Z3SeqAt(seq, offset) => getExpr(e, "seq.at", seq, offset)
        case Z3SeqNth(seq, offset) => getExpr(e, "seq.nth", seq, offset)
        case Z3SeqContains(seq, subseq) => getExpr(e, "seq.contains", seq, subseq)
        case Z3SeqPrefixOf(pre, subseq) => getExpr(e, "seq.prefixof", pre, subseq)
        case Z3SeqSuffixOf(post, seq) => getExpr(e, "seq.suffixof", post, seq)
        case Z3SeqReplace(haystack, needle, replacement) => getExpr(e, "seq.replace", haystack, needle, replacement)
        case Z3SeqMap(f, seq) => getExpr(e, "seq.map", f, seq)
        case Z3SeqMapI(f, offset, seq) => getExpr(e, "seq.mapi", f, offset, seq)
        case Z3SeqFoldl(f, base, seq) => getExpr(e, "seq.foldl", f, base, seq)
        case Z3SeqFoldlI(f, offset, base, seq) => getExpr(e, "seq.foldli", f, offset, base, seq)
        case Z3TransitiveClosure(ref, args) =>
          getExpr(e, s"(_ transitive-closure ${ref.ref.decl.o.preferredName})", args: _*)
      }
    case other => rewriteDefault(other)
  }
}
