package vct.col.newrewrite

import hre.lang.System.Warning
import hre.util.ScopedStack
import vct.col.ast.{Star, _}
import vct.col.ast.util.SuccessionMap
import vct.col.ast.RewriteHelpers._

import scala.collection.mutable

case class CheckProcessAlgebra() extends Rewriter {
  case class ModelPostconditionFailed(process: ModelProcess) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit = process.blame.blame(error)
  }

  case class InsufficientPermissionForModelField(modelDeref: ModelDeref) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit = modelDeref.blame.blame(ModelInsufficientPermission(modelDeref))
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

      val newClass = currentModel.having(model) {
        new Class(
          collectInScope(classScopes) {
            model.declarations.foreach(dispatch(_))
          },
          Seq()
        )(model.o)
      }
      newClass.declareDefault(this)
      modelSuccessors(currentModel) = newClass

    case process: ModelProcess =>
      implicit val o = process.o

      val currentThis = AmbiguousThis()
      currentThis.ref = Some(TClass(modelSuccessors.ref(currentModel.top)))

      def fieldRefToPerm(mode: ModelFieldMode)(implicit o: Origin) = mode match {
        case ModelFieldModifies(_) => WritePerm()
        // TODO: This used to be a permission value passed by argument, so if the read form doesn't work out, this needs to be fixed
        case ModelFieldAccessible(_) => ReadPerm()
      }

      val fieldPerms =
        Star.fold(process.requiredFields.map(f => {
          implicit val o = f.o
          // No blame here because we assume, from the structure we generate these perms, they cannot fail
          // TODO: Is this the right pattern?
          Perm(Deref(currentThis, modelFieldSuccessors.ref(f.f.decl))(null), fieldRefToPerm(f))
        }))

      val args = collectInScope(variableScopes)(process.args.foreach(dispatch(_)))

      new InstanceMethod(
        TVoid(),
        args,
        Seq(),
        None, // TODO: Body
        ApplicableContract(
          // TODO: Is reusing fieldPerms allowed?
          Star(fieldPerms, rewriteDefault(process.requires)),
          Star(fieldPerms, rewriteDefault(process.ensures)),
          Constant.BooleanValue(true),
          Seq(),
          Seq(),
          Seq()
        ),
        false,
        false
      )(ModelPostconditionFailed(process)).declareDefault(this)

    case modelField: ModelField =>
      val instanceField = new InstanceField(modelField.t, Set())(modelField.o)
      instanceField.declareDefault(this)
      modelFieldSuccessors(modelField) = instanceField

    case modelDeref: ModelDeref =>
      implicit val o = modelDeref.o
      val blame = InsufficientPermissionForModelField(modelDeref)
      Deref(dispatch(modelDeref.obj), modelFieldSuccessors.ref(modelDeref.ref.decl))(blame)

    case _ =>
  }
}
