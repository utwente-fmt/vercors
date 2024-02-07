package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.{Blame, DiagnosticOrigin, InsufficientPermission, Origin}
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.LedgerHelper.LedgerMethodBuilderHelper

import scala.collection.immutable.Seq

object CreateLedger extends RewriterBuilder {
  override def key: String = "createLedger"

  override def desc: String = "Create Ledger to store permissions in"

}


case class CreateLedger[Pre <: Generation]() extends Rewriter[Pre] {

  var newClass: Class[Post] = _

  def getNewClass: Class[Post] = newClass

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    lazy val declarations = globalDeclarations.dispatch(program.declarations)
    lazy val ledgerClass = globalDeclarations.collectScoped(createLedger)._1
    val test = program.rewrite(declarations = ledgerClass ++ declarations)
    test
  }

  /*
    class Ledger {
      ...
    }
   */
  def createLedger: Class[Post] = {
    implicit val classOrigin: Origin = Origin(Seq.empty).addLedgerClass().addPrefName("LedgerRuntime")
    val newDeclarations: Seq[ClassDeclaration[Post]] = classDeclarations.collect {
      val creatorMethods: Seq[LedgerMethodBuilderHelper[Post] => Unit] = Seq(createObjectLedger, createLocationLedger,createJoinTokensLedger, createHashMapMethod, createGetPermission, createGetPermissionWithLocation, createGetJoinToken, createSetJoinToken, createSetPermission, createSetPermissionWithLocation, createInitiatePermissionWithSize, createInitiatePermission)
      creatorMethods.map(m => m(LedgerMethodBuilderHelper[Post](new LazyRef[Post, Class[Post]](getNewClass), classDeclarations.freezeBuffer)))
    }._1
    newClass = new Class[Post](newDeclarations, Seq.empty, tt)
    globalDeclarations.declare(newClass)
  }

  /*
        public static ConcurrentHashMap<Long, ConcurrentHashMap<Object, Double>> __runtime__ = new ConcurrentHashMap<Long, ConcurrentHashMap<Object, Double>>();
   */
  def createObjectLedger(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    val fieldFlags: Set[FieldFlag[Post]] = Set(Static[Post]()(DiagnosticOrigin))
    val newInstanceField: InstanceField[Post] = new InstanceField[Post](
      mbh.ledgerProperties.outerHM,
      fieldFlags,
      Some(mbh.ledgerProperties.newOuterMap)
    )(DiagnosticOrigin.addPrefName("__runtime__"))
    classDeclarations.declare(newInstanceField)
  }

  /*
      public static ConcurrentHashMap<Object, ConcurrentHashMap<Integer, Object>> __array_locations__ = new ConcurrentHashMap<Object, ConcurrentHashMap<Integer, Object>>();
   */
  def createLocationLedger(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    val fieldFlags: Set[FieldFlag[Post]] = Set(Static[Post]()(DiagnosticOrigin))
    val newInstanceField: InstanceField[Post] = new InstanceField[Post](
      mbh.ledgerLocationProperties.outerHM,
      fieldFlags,
      Some(mbh.ledgerLocationProperties.newOuterMap)
    )(DiagnosticOrigin.addPrefName("__array_locations__"))
    classDeclarations.declare(newInstanceField)
  }

  /*
      public static ConcurrentHashMap<Object, Fraction> __join_tokens__ = new ConcurrentHashMap<Object, Fraction>();
   */
  def createJoinTokensLedger(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    val fieldFlags: Set[FieldFlag[Post]] = Set(Static[Post]()(DiagnosticOrigin))
    val newInstanceField: InstanceField[Post] = new InstanceField[Post](
      mbh.ledgerJoinTokensProperites.outerHM,
      fieldFlags,
      Some(mbh.ledgerJoinTokensProperites.newOuterMap)
    )(DiagnosticOrigin.addPrefName("__join_tokens__"))
    classDeclarations.declare(newInstanceField)
  }

  /*
      public static void createHashMap() {
        if (!__runtime__.containsKey(Thread.currentThread().getId())) {
            __runtime__.put(Thread.currentThread().getId(), new ConcurrentHashMap<Object, Double>());
        }
      }
   */
  def createHashMapMethod(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    implicit val o: Origin = DiagnosticOrigin
    val containsKey: Expr[Post] = mbh.ledgerProperties.containsKey(mbh.threadId)
    val putNewMap: Expr[Post] = mbh.ledgerProperties.put(mbh.threadId, mbh.ledgerProperties.newInnerMap)
    val keyBranch: Branch[Post] = Branch[Post](Seq((!containsKey, Eval[Post](putNewMap))))

    val newBlock: Block[Post] = Block[Post](Seq(keyBranch))
    val body = Scope[Post](Seq.empty, newBlock)
    classDeclarations.declare(mbh.createMethod(TVoid(), Nil, Some(body), "createHashMap"))
  }

  /*
      public static Double getPermission(Object input) {
          createHashMap();
          return __runtime__.get(Thread.currentThread().getId()).getOrDefault(input, 0.0);
      }
   */
  def createGetPermission(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    implicit val o: Origin = DiagnosticOrigin
    val chm: Eval[Post] = Eval[Post](mbh.miCreateHashMaps.get)
    val input = new Variable[Post](TAnyClass[Post]())(o.addPrefName("input"))
    val returnStat = Return[Post](mbh.ledgerProperties.getOrDefault(mbh.ledgerProperties.get(mbh.threadId), input.get, RuntimeFractionZero[Post]()))
    val newBlock: Block[Post] = Block[Post](Seq(chm, returnStat))
    val body = Scope[Post](Nil, newBlock)
    classDeclarations.declare(mbh.createMethod(TRuntimeFraction[Post](), Seq(input), Some(body), "getPermission"))
  }

  /*
      public static Double getPermission(Object input, int location) {
          createHashMap();
          Object permLoc = __array_locations.get(input).get(location);
          return getPermission(permLoc);
      }
   */
  def createGetPermissionWithLocation(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    implicit val o: Origin = DiagnosticOrigin
    val chm: Eval[Post] = Eval[Post](mbh.miCreateHashMaps.get)
    val inputParam: Variable[Post] = new Variable[Post](TAnyClass[Post]())(o.addPrefName("input"))
    val locationParam: Variable[Post] = new Variable[Post](TInt[Post]())(o.addPrefName("location"))
    val permLoc: Variable[Post] = new Variable[Post](TAnyClass[Post]())(o.addPrefName("permLoc"))
    val assignPermLoc = Assign[Post](permLoc.get, mbh.ledgerLocationProperties.get(mbh.ledgerLocationProperties.get(inputParam.get), locationParam.get))(null)
    val returnStat: Return[Post] = Return[Post](mbh.miGetPermission(permLoc.get).get)
    val newBlock: Block[Post] = Block[Post](Seq(chm, assignPermLoc, returnStat))
    val body = Scope[Post](Seq(permLoc), newBlock)
    classDeclarations.declare(mbh.createMethod(TRuntimeFraction[Post](), Seq(inputParam, locationParam), Some(body), "getPermission"))
  }

  /*
        public static Fraction getPermission(Object input) {
            __runtime__.get(Thread.currentThread().getId()).put(input, value);
        }
   */
  def createGetJoinToken(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    implicit val o: Origin = DiagnosticOrigin
    val input = new Variable[Post](TAnyClass[Post]())(o.addPrefName("input"))
    val returnStat = Return[Post](mbh.ledgerJoinTokensProperites.get(input.get))
    val newBlock: Block[Post] = Block[Post](Seq(returnStat))
    val body = Scope[Post](Nil, newBlock)
    classDeclarations.declare(mbh.createMethod(TRuntimeFraction[Post](), Seq(input), Some(body), "getJoinToken"))
  }


  /*
        public static void setPermission(Object input, Fraction value) {
            assert (value >= 0 && value <= 1) : "value is not between bounds 0 and 1: " + value;
            __runtime__.get(Thread.currentThread().getId()).put(input, value);
        }
   */
  def createSetJoinToken(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    implicit val o: Origin = DiagnosticOrigin
    val inputParam: Variable[Post] = new Variable[Post](TAnyClass[Post]())(o.addPrefName("input"))
    val valueParam: Variable[Post] = new Variable[Post](TRuntimeFraction[Post]())(o.addPrefName("value"))
    val newAssert: RuntimeAssert[Post] = RuntimeAssert[Post]((valueParam.get r_<=> RuntimeFractionZero[Post]()) !== const(-1), "Join token cannot be below 0")(null)
    val putPermission: Eval[Post] = Eval[Post](mbh.ledgerJoinTokensProperites.put(inputParam.get, valueParam.get))
    val newBlock: Block[Post] = Block[Post](Seq(newAssert, putPermission))
    val body = Scope[Post](Nil, newBlock)
    classDeclarations.declare(mbh.createMethod(TVoid(), Seq(inputParam, valueParam), Some(body), "setJoinToken"))
  }

  /*
        public static void setPermission(Object input, Double value) {
            assert (value >= 0 && value <= 1) : "value is not between bounds 0 and 1: " + value;
            createHashMap();
            __runtime__.get(Thread.currentThread().getId()).put(input, value);
        }
   */
  def createSetPermission(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    implicit val o: Origin = DiagnosticOrigin
    val inputParam: Variable[Post] = new Variable[Post](TAnyClass[Post]())(o.addPrefName("input"))
    val valueParam: Variable[Post] = new Variable[Post](TRuntimeFraction[Post]())(o.addPrefName("value"))
    val newAssertLowerBound: RuntimeAssert[Post] = RuntimeAssert[Post]((valueParam.get r_<=> RuntimeFractionZero[Post]()) !== const(-1), "Permission cannot be below 0")(null)
    val newAssertUpperBound: RuntimeAssert[Post] = RuntimeAssert[Post]((valueParam.get r_<=> RuntimeFractionOne[Post]()) !== const(1), "Permisison cannot exceed 1")(null)

    //    val newAssert: RuntimeAssert[Post] = RuntimeAssert[Post](valueParam.get >= RuntimeFractionZero[Post]() && valueParam.get <= RuntimeFractionOne[Post](), "\"value is not between bounds 0 and 1\"")(null)
    val chm: Eval[Post] = Eval[Post](mbh.miCreateHashMaps.get)
    val putPermission: Eval[Post] = Eval[Post](mbh.ledgerProperties.put(mbh.ledgerProperties.get(mbh.threadId), inputParam.get, valueParam.get))
    val newBlock: Block[Post] = Block[Post](Seq(newAssertLowerBound, newAssertUpperBound, chm, putPermission))
    val body = Scope[Post](Nil, newBlock)
    classDeclarations.declare(mbh.createMethod(TVoid(), Seq(inputParam, valueParam), Some(body), "setPermission"))
  }

  /*
        public static void setPermission(Object input, int location, Double value) {
            Object permLoc = __array_locations.get(input).get(location);
            setPermission(permLoc, value);
        }
   */
  def createSetPermissionWithLocation(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    implicit val o: Origin = DiagnosticOrigin
    val inputParam: Variable[Post] = new Variable[Post](TAnyClass[Post]())(o.addPrefName("input"))
    val locationParam: Variable[Post] = new Variable[Post](TInt[Post]())(o.addPrefName("location"))
    val valueParam: Variable[Post] = new Variable[Post](TRuntimeFraction[Post]())(o.addPrefName("value"))
    val permLoc: Variable[Post] = new Variable[Post](TAnyClass[Post]())(o.addPrefName("permLoc"))
    val assignPermLoc = Assign[Post](permLoc.get, mbh.ledgerLocationProperties.get(mbh.ledgerLocationProperties.get(inputParam.get), locationParam.get))(null)
    val setPermission = Eval[Post](mbh.miSetPermission(permLoc.get, valueParam.get).get)
    val newBlock: Block[Post] = Block[Post](Seq(assignPermLoc, setPermission))
    val body = Scope[Post](Seq(permLoc), newBlock)
    classDeclarations.declare(mbh.createMethod(TVoid(), Seq(inputParam, locationParam, valueParam), Some(body), "setPermission"))
  }

  /*
        public static void initiatePermission(Object input, int size) {
            createHashMap();
            setPermission(input, 1.0);
            __array_locations.put(input, new ConcurrentHashMap<>());
            for (int i = 0; i < size; i++) {
                Object[] permLoc = {input, i};
                __array_locations.get(input).put(i, permLoc);
                setPermission(permLoc, 1.0);
            }
        }
   */
  def createInitiatePermissionWithSize(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    implicit val o: Origin = DiagnosticOrigin
    val inputParam: Variable[Post] = new Variable[Post](TAnyClass[Post]())(o.addPrefName("input"))
    val sizeParam: Variable[Post] = new Variable[Post](TInt[Post]())(o.addPrefName("size"))
    val chm: Eval[Post] = Eval[Post](mbh.miCreateHashMaps.get)
    val setPermissionOuter = Eval[Post](mbh.miSetPermission(inputParam.get, RuntimeFractionOne[Post]()).get)
    val createNewConcurrentHashMap = Eval[Post](mbh.ledgerLocationProperties.put(inputParam.get, mbh.ledgerLocationProperties.newInnerMap))

    //Loop statements:
    val permLoc: Variable[Post] = new Variable[Post](TArray[Post](TAnyClass[Post]()))(o.addPrefName("permLoc"))
    val i: Variable[Post] = new Variable[Post](TInt[Post]())(o.addPrefName("i"))
    val assignPermLoc = Eval[Post](PostAssignExpression[Post](permLoc.get, CreateObjectArray[Post](Seq(inputParam.get, i.get)))(null))
    val pushPermLoc = Eval[Post](mbh.ledgerLocationProperties.put(mbh.ledgerLocationProperties.get(inputParam.get), i.get, permLoc.get))
    val setPermission = Eval[Post](mbh.miSetPermission(permLoc.get, RuntimeFractionOne[Post]()).get)
    val loopBody = Block[Post](Seq(assignPermLoc, pushPermLoc, setPermission))
    val newLoop = Loop[Post](
      Assign[Post](i.get, const(0))(null),
      i.get < sizeParam.get,
      Assign[Post](i.get, i.get + const(1))(null),
      LoopInvariant[Post](tt, None)(null),
      loopBody
    )
    val newBlock: Block[Post] = Block[Post](Seq(chm, setPermissionOuter, createNewConcurrentHashMap, newLoop))
    val body = Scope[Post](Seq(permLoc, i), newBlock)
    classDeclarations.declare(mbh.createMethod(TVoid(), Seq(inputParam, sizeParam), Some(body), "initiatePermission"))
  }

  /*
        public static void initiatePermission(Object input) {
            createHashMap();
            setPermission(input, 1.0);
            if (input.getClass().isArray()) {
                initiatePermission(input, Array.getLength(input));
            }
        }
   */
  def createInitiatePermission(mbh: LedgerMethodBuilderHelper[Post]): Unit = {
    implicit val o: Origin = DiagnosticOrigin
    val inputParam: Variable[Post] = new Variable[Post](TAnyClass[Post]())(o.addPrefName("input"))
    val chm: Eval[Post] = Eval[Post](mbh.miCreateHashMaps.get)
    val setPermissionOuter = Eval[Post](mbh.miSetPermission(inputParam.get, RuntimeFractionOne[Post]()).get)
    val branchExpr = ObjectIsArray[Post](inputParam.get)
    val branchBody = Block[Post](Seq(Eval[Post](mbh.miInitiatePermission(inputParam.get, ObjectGetLength[Post](inputParam.get)).get)))
    val branch: Branch[Post] = Branch[Post](Seq((branchExpr, branchBody)))
    val newBlock: Block[Post] = Block[Post](Seq(chm, setPermissionOuter, branch))
    val body = Scope[Post](Nil, newBlock)
    classDeclarations.declare(mbh.createMethod(TVoid(), Seq(inputParam), Some(body), "initiatePermission"))
  }




}