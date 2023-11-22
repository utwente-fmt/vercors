package vct.col.ast.helpers.defn

import vct.col.ast.structure.{Name, Type}

object Types {
  object PrimitiveType {
    private val primitiveFqTypes = Set(
      Constants.ScNothing,
      Constants.ScUnit,
      Constants.ScBoolean,
      Constants.ScByte,
      Constants.ScShort,
      Constants.ScInt,
      Constants.ScLong,
      Constants.ScFloat,
      Constants.ScDouble,
      Constants.ScChar,
      Constants.BigIntPredef,
      Constants.BigDecimalPredef,
      Constants.ScString,
      Constants.ScBigInt,
      Constants.ScBigDecimal,
      Constants.BitString,
    )

    private val primitiveTypes = primitiveFqTypes.flatMap(name => Seq(name.baseName, name))

    def unapply(t: Type): Boolean = t match {
      case Type.Other(name, args) => args.isEmpty && primitiveTypes.contains(name)
      case _ => false
    }
  }

  object OptionType {
    private val optionTypes = Set(Constants.ScOption.baseName, Constants.ScOption)

    def unapply(t: Type): Option[Type] = t match {
      case Type.Other(name, Seq(arg)) if optionTypes.contains(name) => Some(arg)
      case _ => None
    }
  }

  object EitherType {
    private val eitherTypes = Set(Constants.ScEither.baseName, Constants.ScEither, Constants.ScEitherPredef)

    def unapply(t: Type): Option[(Type, Type)] = t match {
      case Type.Other(name, Seq(left, right)) if eitherTypes.contains(name) => Some((left, right))
      case _ => None
    }
  }

  object SeqType {
    private val seqTypes = Set(Constants.ScSeq.baseName, Constants.ScSeq, Constants.ScSeqPredef)

    def unapply(t: Type): Option[Type] = t match {
      case Type.Other(name, Seq(arg)) if seqTypes.contains(name) => Some(arg)
      case _ => None
    }
  }

  object TupleTypes {
    def unapply(t: Type): Option[Seq[Type]] = t match {
      case Type.Tuple(ts) => Some(ts)
      case Type.Other(name, ts) if Constants.ScTupleMin <= ts.size && ts.size <= Constants.ScTupleMax && name == Constants.ScTuple(ts.size) =>
        Some(ts)
      case _ => None
    }
  }
}
