import scala.meta._

case class ColHelperSuccessorsProvider(info: ColDescription) {
  def make(): List[Stat] = q"""
    import vct.col.util.Scopes
    import scala.reflect.ClassTag
    import vct.col.ref.LazyRef

    trait SuccessorsProvider[Pre, Post] {
      ..${ColDefs.DECLARATION_KINDS.map(decl => q"""
        def computeSucc(decl: ${Type.Name(decl)}[Pre]): Option[${Type.Name(decl)}[Post]]
      """).toList}

      ..${ColDefs.DECLARATION_KINDS.map(decl => q"""
        def succ[RefDecl <: Declaration[Post]](decl: ${Type.Name(decl)}[Pre])(implicit tag: ClassTag[RefDecl]): Ref[Post, RefDecl] =
          new LazyRef[Post, RefDecl](computeSucc(decl).get)
      """).toList}
    }

    case class SuccessorsProviderChain[A, B, C](left: SuccessorsProvider[A, B], right: SuccessorsProvider[B, C]) extends SuccessorsProvider[A, C] {
      ..${ColDefs.DECLARATION_KINDS.map(decl => q"""
        def computeSucc(decl: ${Type.Name(decl)}[A]): Option[${Type.Name(decl)}[C]] =
          left.computeSucc(decl).flatMap(right.computeSucc)
      """).toList}
    }

    abstract class SuccessorsProviderTrafo[Pre, Post](inner: SuccessorsProvider[Pre, Post]) extends SuccessorsProvider[Pre, Post] {
      def preTransform[I <: Declaration[Pre], O <: Declaration[Post]](pre: I): Option[O] = None
      def postTransform[T <: Declaration[Post]](pre: Declaration[Pre], post: Option[T]): Option[T] = post

      ..${ColDefs.DECLARATION_KINDS.map(decl => q"""
        def computeSucc(decl: ${Type.Name(decl)}[Pre]): Option[${Type.Name(decl)}[Post]] =
          preTransform(decl).orElse(postTransform(decl, inner.computeSucc(decl)))
      """).toList}
    }
  """.stats
}
