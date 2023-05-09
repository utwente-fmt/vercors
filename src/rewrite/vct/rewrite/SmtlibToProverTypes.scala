package vct.rewrite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}

import scala.collection.mutable

case object SmtlibToProverTypes extends RewriterBuilder {
  override def key: String = "smtlib"
  override def desc: String = "Encode smtlib types and functions into their stringified counterpart"
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
  }

  val declaredType: mutable.Map[String, ProverType[Post]] = mutable.Map()
  val declaredFunc: mutable.Map[String, ProverFunction[Post]] = mutable.Map()

  def getType(t: Type[Pre]): TProverType[Post] = {
    val asText = smtTypeString(t)
    TProverType(declaredType.getOrElseUpdate(asText, {
      implicit val o: Origin = t.o
      globalDeclarations.declare(new ProverType[Post](Seq(SmtLib[Post]() -> asText)))
    }).ref)
  }

  def getExpr(f: String, args: Expr[Pre]*)(implicit o: Origin): Expr[Post] =
    ProverFunctionInvocation(declaredFunc.getOrElseUpdate(f, {
      globalDeclarations.declare(new ProverFunction[Post](Seq(SmtLib[Post]() -> f), ))
    }))

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case t: SmtlibType[Pre] => getType(t)
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case e: SmtlibExpr[Pre] =>
      implicit val o: Origin = e.o
      e match {
        case SmtlibSelect(arr, is) => ???
        case SmtlibStore(arr, is, x) => ???
        case SmtlibBitvecLiteral(data) => ???
        case SmtlibConcat(left, right) => ???
        case SmtlibExtract(inclusiveEndIndexFromRight, startIndexFromRight, bv) => ???
        case SmtlibBvNot(bv) => ???
        case SmtlibBvAnd(left, right) => ???
        case SmtlibBvOr(left, right) => ???
        case SmtlibBvNeg(bv) => ???
        case SmtlibBvAdd(left, right) => ???
        case SmtlibBvMul(left, right) => ???
        case SmtlibBvUDiv(left, right) => ???
        case SmtlibBvURem(left, right) => ???
        case SmtlibBvShl(left, right) => ???
        case SmtlibBvShr(left, right) => ???
        case SmtlibBvULt(left, right) => ???
        case SmtlibRNE() => ???
        case SmtlibRNA() => ???
        case SmtlibRTP() => ???
        case SmtlibRTN() => ???
        case SmtlibRTZ() => ???
        case SmtlibFp(sign, exponent, mantissa) => ???
        case SmtlibFpAbs(arg) => ???
        case SmtlibFpNeg(arg) => ???
        case SmtlibFpAdd(left, right) => ???
        case SmtlibFpSub(left, right) => ???
        case SmtlibFpMul(left, right) => ???
        case SmtlibFpDiv(left, right) => ???
        case SmtlibFpFma(left, right, addend) => ???
        case SmtlibFpSqrt(arg) => ???
        case SmtlibFpRem(left, right) => ???
        case SmtlibFpRoundToIntegral(arg) => ???
        case SmtlibFpMin(left, right) => ???
        case SmtlibFpMax(left, right) => ???
        case SmtlibFpLeq(left, right) => ???
        case SmtlibFpLt(left, right) => ???
        case SmtlibFpGeq(left, right) => ???
        case SmtlibFpGt(left, right) => ???
        case SmtlibFpEq(left, right) => ???
        case SmtlibFpIsNormal(arg) => ???
        case SmtlibFpIsSubnormal(arg) => ???
        case SmtlibFpIsZero(arg) => ???
        case SmtlibFpIsInfinite(arg) => ???
        case SmtlibFpIsNaN(arg) => ???
        case SmtlibFpIsNegative(arg) => ???
        case SmtlibFpIsPositive(arg) => ???
        case SmtlibToFp(bv, exponentBits, mantissaAndSignBits) => ???
        case SmtlibFpCast(arg, exponentBits, mantissaAndSignBits) => ???
        case SmtlibFpFromReal(arg, exponentBits, mantissaAndSignBits) => ???
        case SmtlibFpFromSInt(bv, exponentBits, mantissaAndSignBits) => ???
        case SmtlibFpFromUInt(bv, exponentBits, mantissaAndSignBits) => ???
        case SmtlibFpToReal(arg) => ???
        case SmtlibFpToSInt(arg, bits) => ???
        case SmtlibFpToUInt(arg, bits) => ???
        case SmtlibLiteralString(data) => ???
        case SmtlibStrConcat(left, right) => ???
        case SmtlibStrLen(arg) => ???
        case SmtlibStrLt(left, right) => ???
        case SmtlibStrLeq(left, right) => ???
        case SmtlibStrAt(str, i) => ???
        case SmtlibSubstr(str, i, n) => ???
        case SmtlibStrPrefixOf(left, right) => ???
        case SmtlibStrSuffixOf(left, right) => ???
        case SmtlibStrContains(left, right) => ???
        case SmtlibStrIndexOf(haystack, needle, fromIndex) => ???
        case SmtlibStrReplace(haystack, needle, replacement) => ???
        case SmtlibStrReplaceAll(haystack, needle, replacement) => ???
        case SmtlibStrReplaceRe(haystack, re, replacement) => ???
        case SmtlibStrReplaceReAll(haystack, re, replacement) => ???
        case SmtlibStrIsDigit(arg) => ???
        case SmtlibStrToCode(arg) => ???
        case SmtlibStrFromCode(arg) => ???
        case SmtlibStrToInt(arg) => ???
        case SmtlibStrFromInt(arg) => ???
        case SmtlibReFromStr(arg) => ???
        case SmtlibReContains(re, str) => ???
        case SmtlibReNone() => ???
        case SmtlibReAll() => ???
        case SmtlibReAllChar() => ???
        case SmtlibReConcat(left, right) => ???
        case SmtlibReUnion(left, right) => ???
        case SmtlibReInter(left, right) => ???
        case SmtlibReStar(arg) => ???
        case SmtlibReComp(arg) => ???
        case SmtlibReDiff(left, right) => ???
        case SmtlibRePlus(arg) => ???
        case SmtlibReOpt(arg) => ???
        case SmtlibReRange(left, right) => ???
        case SmtlibReRepeat(count, arg) => ???
        case SmtlibReRepeatRange(from, to, arg) => ???
        case Z3BvSub(left, right) => ???
        case Z3BvSRem(left, right) => ???
        case Z3BvSMod(left, right) => ???
        case Z3BvSShr(left, right) => ???
        case Z3BvNand(left, right) => ???
        case Z3BvNor(left, right) => ???
        case Z3BvXnor(left, right) => ???
        case Z3ArrayConst(domain, codomain, value) => ???
        case Z3ArrayOfFunction(ref) => ???
        case Z3ArrayMap(ref, args) => ???
        case Z3SeqEmpty(elementType) => ???
        case Z3SeqUnit(arg) => ???
        case Z3SeqConcat(left, right) => ???
        case Z3SeqLen(arg) => ???
        case Z3SeqExtract(seq, offset, len) => ???
        case Z3SeqAt(seq, offset) => ???
        case Z3SeqNth(seq, offset) => ???
        case Z3SeqContains(seq, subseq) => ???
        case Z3SeqPrefixOf(pre, subseq) => ???
        case Z3SeqSuffixOf(post, seq) => ???
        case Z3SeqReplace(haystack, needle, replacement) => ???
        case Z3SeqMap(f, seq) => ???
        case Z3SeqMapI(f, offset, seq) => ???
        case Z3SeqFoldl(f, base, seq) => ???
        case Z3SeqFoldlI(f, offset, base, seq) => ???
        case Z3TransitiveClosure(ref, args) => ???
      }
    case other => rewriteDefault(other)
  }
}
