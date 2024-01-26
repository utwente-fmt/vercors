package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}

object CreateLedger extends RewriterBuilder {
  override def key: String = "createLedger"

  override def desc: String = "Create Ledger to store permissions in"
}

/*

  Ledger template:

  class Ledger {

      public static ConcurrentHashMap<Long, ConcurrentHashMap<Object, Double>> __runtime__ = new ConcurrentHashMap<Long, ConcurrentHashMap<Object, Double>>();
      public static ConcurrentHashMap<Object, ConcurrentHashMap<Integer, Object>> __array_locations = new ConcurrentHashMap<Object, ConcurrentHashMap<Integer, Object>>();


      public static void createHashMap() {
          if (!__runtime__.containsKey(Thread.currentThread().getId())) {
              __runtime__.put(Thread.currentThread().getId(), new ConcurrentHashMap<Object, Double>());
          }
      }

      public static Double getPermission(Object input) {
          createHashMap();
          return __runtime__.get(Thread.currentThread().getId()).getOrDefault(input, 0.0);
      }

      public static Double getPermission(Object input, int location) {
          createHashMap();
          Object permLoc = __array_locations.get(input).get(location);
          return getPermission(permLoc);
      }

      public static void setPermission(Object input, Double value) {
          assert (value >= 0 && value <= 1) : "value is not between bounds 0 and 1: " + value;
          createHashMap();
          __runtime__.get(Thread.currentThread().getId()).put(input, value);
      }

      public static void setPermission(Object input, int location, Double value) {
          createHashMap();
          assert (input.getClass().isArray());
          Object permLoc = __array_locations.get(input).get(location);
          setPermission(permLoc, value);
      }

      public static void initiatePermission(Object input) {
          createHashMap();
          setPermission(input, 1.0);
          if (input.getClass().isArray()) {
              initiatePermission(input, Array.getLength(input));
          }
      }

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
  }
 */






case class CreateLedger[Pre <: Generation]() extends Rewriter[Pre] {

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    val newDecls: Seq[GlobalDeclaration[Post]] = globalDeclarations.collect {
      super.dispatch(program)
      createLedger
    }._1
    program.rewrite(declarations = newDecls)
  }

  def createLedger : Unit = {
    //TODO create ledger
  }
}