import scala.meta._

case class ColHelperSuccessorsProvider(info: ColDescription) {
  def make(): List[Stat] = q"""
    import vct.col.util.Scopes
    import scala.reflect.ClassTag
    import vct.col.ref.LazyRef

    trait SuccessorsProvider[Pre, Post] {
      def anySucc[RefDecl <: Declaration[Post]](decl: Declaration[Pre])(implicit tag: ClassTag[RefDecl]): Ref[Post, RefDecl] =
        ${MetaUtil.NonemptyMatch("decl succ kind cases", q"decl", ColDefs.DECLARATION_KINDS.map(decl =>
          Case(p"decl: ${Type.Name(decl)}[Pre]", None, q"succ(decl)")
        ).toList)}

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
      def preTransform[T <: Declaration[Pre]](pre: T): Option[T] = Some(pre)
      def postTransform[T <: Declaration[Post]](pre: Declaration[Pre], post: Option[T]): Option[T] = post

      ..${ColDefs.DECLARATION_KINDS.map(decl => q"""
        def computeSucc(decl: ${Type.Name(decl)}[Pre]): Option[${Type.Name(decl)}[Post]] =
          postTransform(decl, preTransform(decl).flatMap(inner.computeSucc))
      """).toList}
    }
  """.stats
}
