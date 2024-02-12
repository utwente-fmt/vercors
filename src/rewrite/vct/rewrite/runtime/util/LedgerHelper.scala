package vct.rewrite.runtime.util

import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter}
import vct.result.VerificationError.Unreachable

import scala.collection.immutable.Nil

object LedgerHelper {



  def findNumberInstanceField[G](program: Program[G], instanceField: InstanceField[G]): Option[Int] = {
    program
      .declarations
      .collect{case cls: Class[G] => cls}
      .map(cls => cls.declarations)
      .map(decls => decls.collect{case ifd: InstanceField[G] => ifd})
      .map(ifs => ifs.indexOf(instanceField))
      .find(i => i >= 0)
  }


  case class LedgerRewriter[Pre <: Generation](outer: Rewriter[Pre]) extends Rewriter[Pre] {
    override val allScopes = outer.allScopes

    def rewriteLedger(program: Program[Pre]): (LedgerMethodBuilderHelper[Post], Class[Post], Seq[GlobalDeclaration[Pre]]) = {
      val (Seq(ledgerClass: Class[Pre]), otherDeclarations: Seq[GlobalDeclaration[Pre]]) = program.declarations.partition {
        case cls: Class[Pre] if cls.o.getLedgerClassRuntime.nonEmpty => true
        case _ => false
      }
      val clsDecl = classDeclarations.collect(dispatch(ledgerClass))._1
      val ledgerRef: Ref[Post, Class[Post]] = this.anySucc(ledgerClass)
      val newClass = ledgerRef.decl
      (LedgerMethodBuilderHelper[Post](ledgerRef, newClass.declarations), newClass, otherDeclarations)
    }
  }

  case class LedgerProperties[G](innerHM: Type[G], outerHM: Type[G], refCls: Ref[G, Class[G]], ledger: Option[InstanceField[G]])(implicit origin: Origin = DiagnosticOrigin) {
    def deref: Option[Deref[G]] = ledger.map(l => Deref[G](StaticClassRef[G](refCls), l.ref)(null)(l.o))

    def get(hm: Expr[G], key: Expr[G]): RuntimeConcurrentHashMapGet[G] = RuntimeConcurrentHashMapGet[G](hm, key)
    def get(key: Expr[G]): RuntimeConcurrentHashMapGet[G] = RuntimeConcurrentHashMapGet[G](deref.get, key)

    def getOrDefault(hm: Expr[G], key: Expr[G], default: Expr[G]): RuntimeConcurrentHashMapGetOrDefault[G] = RuntimeConcurrentHashMapGetOrDefault[G](hm, key, default)
    def getOrDefault(key: Expr[G], default: Expr[G]): RuntimeConcurrentHashMapGetOrDefault[G] = RuntimeConcurrentHashMapGetOrDefault[G](deref.get, key, default)

    def put(hm: Expr[G], key: Expr[G], value: Expr[G]): RuntimeConcurrentHashMapPut[G] = RuntimeConcurrentHashMapPut[G](hm, key, value)
    def put(key: Expr[G], value: Expr[G]): RuntimeConcurrentHashMapPut[G] = RuntimeConcurrentHashMapPut[G](deref.get, key, value)

    def containsKey(hm: Expr[G], key: Expr[G]): RuntimeConcurrentHashMapContainsKey[G] = RuntimeConcurrentHashMapContainsKey[G](hm, key)
    def containsKey(key: Expr[G]): RuntimeConcurrentHashMapContainsKey[G] = RuntimeConcurrentHashMapContainsKey[G](deref.get, key)

    def newInnerMap: RuntimeNewConcurrentHashMap[G] = RuntimeNewConcurrentHashMap[G](innerHM)
    def newOuterMap: RuntimeNewConcurrentHashMap[G] = RuntimeNewConcurrentHashMap[G](outerHM)

  }

  object LedgerMethodBuilderHelper{
    def apply[G](program: Program[G]) : LedgerMethodBuilderHelper[G] = {
      val cls = program.declarations.collectFirst{case cls: Class[G] if cls.o.getLedgerClassRuntime.nonEmpty => cls}.get
      LedgerMethodBuilderHelper[G](cls.ref, cls.declarations)
    }
  }

  case class LedgerMethodBuilderHelper[G](refCls: Ref[G, Class[G]], clsDeclarations: Seq[ClassDeclaration[G]])(implicit origin: Origin = DiagnosticOrigin) {
    def threadId: ThreadId[G] = ThreadId[G](None)(DiagnosticOrigin)

    private def findAllMethods(methodName: String): Seq[InstanceMethod[G]] = clsDeclarations.collect { case i: InstanceMethod[G] if i.o.getPreferredNameOrElse() == methodName => i }
    private def findMethod(methodName: String): Option[InstanceMethod[G]] = findAllMethods(methodName).headOption
    private def findMethod(methodName: String, params: Int): Option[InstanceMethod[G]] = findAllMethods(methodName).find(i => i.args.size == params)

    private def findInstanceField(instanceFieldName: String): Option[InstanceField[G]] = clsDeclarations.collectFirst { case i: InstanceField[G] if i.o.getPreferredNameOrElse() == instanceFieldName => i }

    def ledgerProperties: LedgerProperties[G] = LedgerProperties[G](
      RuntimeConcurrentHashMap[G](TAnyClass[G](), TRuntimeFraction[G]())(DiagnosticOrigin),
      RuntimeConcurrentHashMap[G](TLongObject[G](), RuntimeConcurrentHashMap[G](TAnyClass[G](), TRuntimeFraction[G]())(DiagnosticOrigin))(DiagnosticOrigin),
      refCls,
      findInstanceField("__runtime__")
    )

    def ledgerLocationProperties: LedgerProperties[G] = LedgerProperties[G](
      RuntimeConcurrentHashMap[G](TIntObject[G](), TAnyClass[G]())(DiagnosticOrigin),
      RuntimeConcurrentHashMap[G](TAnyClass[G](), RuntimeConcurrentHashMap[G](TIntObject[G](), TAnyClass[G]())(DiagnosticOrigin))(DiagnosticOrigin),
      refCls,
      findInstanceField("__array_locations__")
    )

    def ledgerJoinTokensProperites: LedgerProperties[G] = LedgerProperties[G](
      TRuntimeFraction[G](),
      RuntimeConcurrentHashMap[G](TAnyClass[G](), TRuntimeFraction[G]())(DiagnosticOrigin),
      refCls,
      findInstanceField("__join_tokens__")
    )

    def createHashMaps: Option[InstanceMethod[G]] = findMethod("createHashMap")
    def miCreateHashMaps: Option[MethodInvocation[G]] = createMethodInvocation(createHashMaps, Nil)

    def getPermission(params: Int): Option[InstanceMethod[G]] = findMethod("getPermission", params)
    def miGetPermission(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(getPermission(1), Seq(input))
    def miGetPermission(input: Expr[G], location: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(getPermission(2), Seq(input, location))

    def getJoinToken: Option[InstanceMethod[G]] = findMethod("getJoinToken")
    def miGetJoinToken(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(getJoinToken, Seq(input))

    def setJoinToken: Option[InstanceMethod[G]] = findMethod("setJoinToken")
    def miSetJoinToken(input: Expr[G], value: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(setJoinToken, Seq(input, value))

    def setPermission(params: Int): Option[InstanceMethod[G]] = findMethod("setPermission", params)
    def miSetPermission(input: Expr[G], value: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(setPermission(2), Seq(input, value))
    def miSetPermission(input: Expr[G], location: Expr[G], value: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(setPermission(3), Seq(input,location, value))

    def initiatePermission: Option[InstanceMethod[G]] = findMethod("initiatePermission", 2)
    def initiatePermission(params: Int): Option[InstanceMethod[G]] = findMethod("initiatePermission", params)
    def miInitiatePermission(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(initiatePermission(1), Seq(input))
    def miInitiatePermission(input: Expr[G], size: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(initiatePermission(2), Seq(input, size))

    def createMethodInvocation(imo: Option[InstanceMethod[G]], args: Seq[Expr[G]]): Option[MethodInvocation[G]] = imo.map(im => MethodInvocation[G](
      StaticClassRef[G](refCls),
      im.ref,
      args,
      Nil,
      Nil,
      Nil,
      Nil)(null)(im.o))

    def createMethod(returnType: Type[G], args: Seq[Variable[G]], body: Option[Scope[G]], methodName: String): InstanceMethod[G] = {
      new InstanceMethod(
        returnType,
        args,
        Nil,
        Nil,
        body,
        ApplicableContract.createEmptyContract,
        static = true
      )(null)(DiagnosticOrigin.addPrefName(methodName).addLedgerClass())
    }
  }

}