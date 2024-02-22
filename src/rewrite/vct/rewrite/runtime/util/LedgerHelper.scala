package vct.rewrite.runtime.util

import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter}
import vct.result.VerificationError.Unreachable

import scala.collection.mutable.ArrayBuffer

object LedgerHelper {

  def findNumberInstanceField[G](program: Program[G], instanceField: InstanceField[G]): Option[Int] = {
    program
      .declarations
      .collect { case cls: Class[G] => cls }
      .map(cls => cls.declarations)
      .map(decls => decls.collect { case ifd: InstanceField[G] => ifd })
      .map(ifs => ifs.indexOf(instanceField))
      .find(i => i >= 0)
  }


  case class LedgerRewriter[Pre <: Generation](outer: Rewriter[Pre]) extends Rewriter[Pre] {
    override val allScopes = outer.allScopes

    def rewriteLedger(program: Program[Pre]): (LedgerMethodBuilderHelper[Post], Seq[Class[Post]], Seq[GlobalDeclaration[Pre]]) = {
      val (Seq(ledgerClass: Class[Pre]), od: Seq[GlobalDeclaration[Pre]]) = program.declarations.partition {
        case cls: Class[Pre] if cls.o.getLedgerClassRuntime.nonEmpty => true
        case _ => false
      }

      val (Seq(predicateClass: Class[Pre]), otherDeclarations: Seq[GlobalDeclaration[Pre]]) = od.partition {
        case cls: Class[Pre] if cls.o.getDataObjectClassRuntime.nonEmpty => true
        case _ => false
      }

      val newClasses = classDeclarations.collect{
        dispatch(ledgerClass)
        dispatch(predicateClass)
      }._1

      val ledgerRef: Ref[Post, Class[Post]] = this.anySucc(ledgerClass)
      val predicateRef: Ref[Post, Class[Post]] = this.anySucc(predicateClass)
      val newClass = ledgerRef.decl
      val pmbh = DataMethodBuilderHelper[Post](predicateRef, predicateRef.decl.declarations)

      (LedgerMethodBuilderHelper[Post](ledgerRef, newClass.declarations, pmbh),
        Seq(ledgerRef.decl, predicateRef.decl),
        otherDeclarations)
    }
  }

  case class LedgerProperties[G](inner: Type[G], outerHM: Type[G], refCls: Ref[G, Class[G]], ledger: Option[InstanceField[G]])(implicit origin: Origin = DiagnosticOrigin) {
    def deref: Option[Deref[G]] = ledger.map(l => Deref[G](StaticClassRef[G](refCls), l.ref)(null)(l.o))

    def get(hm: Expr[G], key: Expr[G]): RuntimeConcurrentHashMapGet[G] = RuntimeConcurrentHashMapGet[G](hm, key)

    def get(key: Expr[G]): RuntimeConcurrentHashMapGet[G] = RuntimeConcurrentHashMapGet[G](deref.get, key)

    def getOrDefault(hm: Expr[G], key: Expr[G], default: Expr[G]): RuntimeConcurrentHashMapGetOrDefault[G] = RuntimeConcurrentHashMapGetOrDefault[G](hm, key, default)

    def getOrDefault(key: Expr[G], default: Expr[G]): RuntimeConcurrentHashMapGetOrDefault[G] = RuntimeConcurrentHashMapGetOrDefault[G](deref.get, key, default)

    def put(hm: Expr[G], key: Expr[G], value: Expr[G]): RuntimeConcurrentHashMapPut[G] = RuntimeConcurrentHashMapPut[G](hm, key, value)

    def put(key: Expr[G], value: Expr[G]): RuntimeConcurrentHashMapPut[G] = RuntimeConcurrentHashMapPut[G](deref.get, key, value)

    def containsKey(hm: Expr[G], key: Expr[G]): RuntimeConcurrentHashMapContainsKey[G] = RuntimeConcurrentHashMapContainsKey[G](hm, key)

    def containsKey(key: Expr[G]): RuntimeConcurrentHashMapContainsKey[G] = RuntimeConcurrentHashMapContainsKey[G](deref.get, key)

    def keySet(hm: Expr[G]): RuntimeConcurrentHashMapKeySet[G] = RuntimeConcurrentHashMapKeySet[G](hm);
    def keySet(): RuntimeConcurrentHashMapKeySet[G] = RuntimeConcurrentHashMapKeySet[G](deref.get);

    def newInner: Expr[G] = {
      inner match {
        case _: RuntimeConcurrentHashMap[G] => RuntimeNewConcurrentHashMap[G](inner)
        case _: CopyOnWriteArrayList[G] => CopyOnWriteArrayListNew[G](inner)
        case _ => throw Unreachable(s"Add implementation for type: ${inner}")
      }
    }

    def newOuterMap: RuntimeNewConcurrentHashMap[G] = RuntimeNewConcurrentHashMap[G](outerHM)


    def add(obj: Expr[G], arg: Expr[G]): CopyOnWriteArrayListAdd[G] = CopyOnWriteArrayListAdd[G](obj, arg)
    def remove(obj: Expr[G], arg: Expr[G]): CopyOnWriteArrayListRemove[G] = CopyOnWriteArrayListRemove[G](obj, arg)
    def contains(obj: Expr[G], arg: Expr[G]): CopyOnWriteArrayListContains[G] = CopyOnWriteArrayListContains[G](obj, arg)

  }

  object LedgerMethodBuilderHelper {
    def apply[G](program: Program[G]): LedgerMethodBuilderHelper[G] = {
      val cls = program.declarations.collectFirst { case cls: Class[G] if cls.o.getLedgerClassRuntime.nonEmpty => cls }.get
      val predicatecls = program.declarations.collectFirst { case cls: Class[G] if cls.o.getDataObjectClassRuntime.nonEmpty => cls }.get
      LedgerMethodBuilderHelper[G](cls.ref, cls.declarations, DataMethodBuilderHelper[G](predicatecls.ref, predicatecls.declarations))
    }
  }

  case class LedgerMethodBuilderHelper[G](refCls: Ref[G, Class[G]], clsDeclarations: Seq[ClassDeclaration[G]], pmbh: DataMethodBuilderHelper[G])(implicit origin: Origin = DiagnosticOrigin) {
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

    def ledgerJoinTokensProperites: LedgerProperties[G] = LedgerProperties[G](
      TRuntimeFraction[G](),
      RuntimeConcurrentHashMap[G](TAnyClass[G](), TRuntimeFraction[G]())(DiagnosticOrigin),
      refCls,
      findInstanceField("__join_tokens__")
    )

    def ledgerPredicateStore: LedgerProperties[G] = LedgerProperties[G](
      CopyOnWriteArrayList[G](TClass[G](pmbh.refCls)),
      RuntimeConcurrentHashMap[G](TLongObject[G](), CopyOnWriteArrayList[G](TClass[G](pmbh.refCls)))(DiagnosticOrigin),
      refCls,
      findInstanceField("__predicate_store__")
    )


    def injectivityMap: LedgerProperties[G] = LedgerProperties[G](
      TRuntimeFraction[G](),
      RuntimeConcurrentHashMap[G](TAnyClass[G](), TRuntimeFraction[G]())(DiagnosticOrigin),
      refCls,
      None
    )

    def createNewInjectivityMap: Variable[G] = new Variable[G](injectivityMap.outerHM)(DiagnosticOrigin.addPrefName("injectivityMap"))


    def createHashMaps: Option[InstanceMethod[G]] = findMethod("createHashMap")

    def miCreateHashMaps: Option[MethodInvocation[G]] = createMethodInvocation(createHashMaps, Nil)

    def getPermission(params: Int): Option[InstanceMethod[G]] = findMethod("getPermission", params)

    def miGetPermission(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(getPermission(1), Seq(input))

    def getJoinToken: Option[InstanceMethod[G]] = findMethod("getJoinToken")

    def miGetJoinToken(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(getJoinToken, Seq(input))

    def setJoinToken: Option[InstanceMethod[G]] = findMethod("setJoinToken")

    def miSetJoinToken(input: Expr[G], value: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(setJoinToken, Seq(input, value))

    def setPermission(params: Int): Option[InstanceMethod[G]] = findMethod("setPermission", params)

    def miSetPermission(input: Expr[G], value: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(setPermission(2), Seq(input, value))

    def initiatePermission: Option[InstanceMethod[G]] = findMethod("initiatePermission", 2)

    def initiatePermission(params: Int): Option[InstanceMethod[G]] = findMethod("initiatePermission", params)

    def miInitiatePermission(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(initiatePermission(1), Seq(input))

    def miInitiatePermission(input: Expr[G], size: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(initiatePermission(2), Seq(input, size))


    def hasPredicateCheck: Option[InstanceMethod[G]] = findMethod("hasPredicateCheck")
    def miHasPredicateCheck(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(hasPredicateCheck, Seq(input))

    def foldPredicate: Option[InstanceMethod[G]] = findMethod("foldPredicate")
    def miFoldPredicate(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(foldPredicate, Seq(input))

    def unfoldPredicate: Option[InstanceMethod[G]] = findMethod("unfoldPredicate")
    def miUnfoldPredicate(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(unfoldPredicate, Seq(input))

    def checkForInjectivity: Option[InstanceMethod[G]] = findMethod("checkForInjectivity")
    def miCheckForInjectivity(input: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(checkForInjectivity, Seq(input))

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


  case class DataMethodBuilderHelper[G](refCls: Ref[G, Class[G]], clsDeclarations: Seq[ClassDeclaration[G]])(implicit origin: Origin = DiagnosticOrigin) {

    private def findAllMethods(methodName: String): Seq[InstanceMethod[G]] = clsDeclarations.collect { case i: InstanceMethod[G] if i.o.getPreferredNameOrElse() == methodName => i }

    private def findMethod(methodName: String): Option[InstanceMethod[G]] = findAllMethods(methodName).headOption

    private def findMethod(methodName: String, params: Int): Option[InstanceMethod[G]] = findAllMethods(methodName).find(i => i.args.size == params)

    private def findInstanceField(instanceFieldName: String): Option[InstanceField[G]] = clsDeclarations.collectFirst { case i: InstanceField[G] if i.o.getPreferredNameOrElse() == instanceFieldName => i }

    def dataField: Option[InstanceField[G]] = findInstanceField("data")

    def setData(params: Int): Option[InstanceMethod[G]] = findMethod("setData", params)

    def miSetData(obj: Expr[G], data: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(setData(1), Seq(data), Some(obj))

    def create: Option[InstanceMethod[G]] = findMethod("create")

    def miCreate(obj: Expr[G]): Option[MethodInvocation[G]] = createMethodInvocation(create, Seq(obj))

    def createMethodInvocation(imo: Option[InstanceMethod[G]], args: Seq[Expr[G]], nonStatic: Option[Expr[G]] = None): Option[MethodInvocation[G]] = imo.map(im => MethodInvocation[G](
      nonStatic.getOrElse(StaticClassRef[G](refCls)),
      im.ref,
        args,
      Nil,
      Nil,
      Nil,
      Nil)(null)(im.o))

    def createMethod(returnType: Type[G], args: Seq[Variable[G]], body: Option[Scope[G]], methodName: String, static: Boolean = true): InstanceMethod[G] = {
      new InstanceMethod(
        returnType,
        args,
        Nil,
        Nil,
        body,
        ApplicableContract.createEmptyContract,
        static = static
      )(null)(DiagnosticOrigin.addPrefName(methodName).addDataObjectClass())
    }
  }




}