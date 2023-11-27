package vct.col.ast.structure

object Constants {
  val RootNodeName: Name = Name(Seq("_root_", "vct", "col", "ast", "Node"))
  val NodeFamilyName: Name = Name(Seq("_root_", "vct", "col", "ast", "NodeFamily"))
  val DeclarationName: Name = Name(Seq("_root_", "vct", "col", "ast", "Declaration"))
  val RefName: Name = Name(Seq("_root_", "vct", "col", "ref", "Ref"))
  val FamilyName: Name = Name(Seq("_root_", "vct", "col", "structure", "family"))

  def typeMap[T](pairs: (Seq[String], T)*): Map[Seq[String], T] =
    pairs.flatMap {
      case name -> cons => Seq(
        Seq(name.last) -> cons,
        name -> cons,
        ("_root_" +: name) -> cons,
      )
    }.toMap

  val ValueTypes: Map[Seq[String], Type.ValueType] = typeMap(
    Seq("scala", "Boolean") -> Type.Boolean,
    Seq("scala", "Byte") -> Type.Byte,
    Seq("scala", "Short") -> Type.Short,
    Seq("scala", "Int") -> Type.Int,
    Seq("scala", "Long") -> Type.Long,
    Seq("scala", "Float") -> Type.Float,
    Seq("scala", "Double") -> Type.Double,
    Seq("scala", "Char") -> Type.Char,
  )

  val PrimitiveTypes: Map[Seq[String], Type.PrimitiveType] = typeMap(
    Seq("scala", "Nothing") -> Type.Nothing,
    Seq("scala", "Unit") -> Type.Unit,
    Seq("java", "lang", "String") -> Type.String,
    Seq("scala", "BigInt") -> Type.BigInt,
    Seq("scala", "math", "BigInt") -> Type.BigInt,
    Seq("scala", "BigDecimal") -> Type.BigDecimal,
    Seq("scala", "math", "BigDecimal") -> Type.BigDecimal,
    Seq("hre", "data", "BitString") -> Type.BitString,
    Seq("vct", "col", "origin", "ExpectedError") -> Type.ExpectedError,
  )

  val CollectionType1: Map[Seq[String], Type => Type] = typeMap(
    Seq("scala", "Seq") -> Type.Seq.apply,
    Seq("scala", "collection", "immutable", "Seq") -> Type.Seq.apply,
    Seq("scala", "Option") -> Type.Option.apply,
  )

  val CollectionType2: Map[Seq[String], (Type, Type) => Type] = typeMap(
    Seq("scala", "Either") -> Type.Either.apply,
  )
}