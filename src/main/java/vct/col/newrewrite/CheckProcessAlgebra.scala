package vct.col.newrewrite

import hre.lang.System.Warning
import hre.util.ScopedStack
import vct.col.ast.{Star, _}
import vct.col.ast.util.SuccessionMap

import scala.collection.mutable

case class CheckProcessAlgebra() extends Rewriter {
  case class CheckProcessAlgebraBlame(process: ModelProcess) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit = {
      error.failure match {
        case InsufficientPermissionToExhale(_) =>
          // TODO: Is there some default blame to forward to? Same for next
          InternalError(s"BUG: Unexpected permission error when exhaling process postcondition: ${error.failure}").toString()
        case NegativePermissionValue(_) =>
          InternalError(s"BUG: Unexpected permission error when exhaling process postcondition: ${error.failure}").toString()
        case ReceiverNotInjective(_) =>
          process.blame.blame(error)
        case ContractFalse(_) =>
          process.blame.blame(error)
      }
    }
  }

  val modelFieldSuccessors: SuccessionMap[ModelField, InstanceField] = SuccessionMap()
  val modelSuccessors: SuccessionMap[Model, Class] = SuccessionMap()
  val currentModel: ScopedStack[Model] = ScopedStack()

  override def dispatch(model: Declaration): Unit = model match {
    //      val x = Function().declareDefault()
    //      model.succeedDefault(this, x)
    //      model.rewrite().declareDefault()
    case model: Model =>
      // We put all permutations of every top-level parallel process
      // in a map to detect overlapping ones
      val compositeMap: mutable.Map[Set[Expr], ModelProcess] = mutable.Map()

      model.declarations.foreach {
        case process: ModelProcess =>
          process.impl match {
            case processPar: ProcessPar =>
              val parallelCompositionElems = processPar.unfoldProcessPar.toSet
              if (parallelCompositionElems.forall(_.isInstanceOf[ProcessApply])) {
                if (compositeMap.contains(parallelCompositionElems)) {
                  Warning(
                    "Collision detected: %s vs. %s have same set of process elements composed in parallel",
                    process.o.preferredName,
                    compositeMap.get(parallelCompositionElems).get.o.preferredName
                  )
                } else {
                  compositeMap.put(parallelCompositionElems, process)
                }
              } else {
                // TODO: Should this be done by typechecking?
                Warning("Process detected that composes non-process elements in parallel")
              }
            case _ =>
          }
        case _ =>
      }

      val `class` = currentModel.having(model) {
        new Class(
          collectInScope(classScopes) {
            model.declarations.foreach(dispatch(_))
          },
          Seq()
        )(model.o)
      }
      `class`.declareDefault(this)
      modelSuccessors(currentModel) = `class`

    case process: ModelProcess =>
      implicit val o = process.o

      val currentThis = AmbiguousThis()
      currentThis.ref = Some(TClass(modelSuccessors.ref(currentModel.top)))

      val modifiesPerm =
        Star.fold(process.requiredFields.map(fieldRef => {
          implicit val o = fieldRef.o
          Perm(Deref(currentThis, modelFieldSuccessors.ref(fieldRef.decl)), WritePerm())(PermissionForModifies(fieldRef.decl))
        }))

      val accessiblePerm = ???
//        Star.fold(process.accessible.map(fieldRef => {
//        implicit val o = fieldRef.decl.o
//        /* TODO: In earlier implementation this was argument based, instead of readPerm based.
//                 Once we can actually test this we can consider the other implementation
//         */
//        Perm(Deref(currentThis, modelFieldSuccessors.ref(fieldRef.decl)), ReadPerm())(PermissionForAccessible(fieldRef.decl))
//      }))

      val args = collectInScope(variableScopes)(process.args.foreach(dispatch(_)))

      new InstanceMethod(
        TVoid(),
        args,
        Seq(),
        None,
        ApplicableContract(
          Star.fold(Seq(accessiblePerm, modifiesPerm, rewriteDefault(process.requires))),
          Star.fold(Seq(accessiblePerm, modifiesPerm, rewriteDefault(process.ensures))),
          Constant.BooleanValue(true),
          Seq(),
          Seq(),
          Seq()
        ),
        false,
        false
      )(CheckProcessAlgebraBlame(process)).declareDefault(this)

    case modelField: ModelField =>
      val instanceField = new InstanceField(modelField.t, Set())(modelField.o)
      instanceField.declareDefault(this)
      modelFieldSuccessors(modelField) = instanceField

    case _ =>
  }
}
