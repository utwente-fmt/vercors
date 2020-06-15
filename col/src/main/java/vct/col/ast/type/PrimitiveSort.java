package vct.col.ast.type;

public enum PrimitiveSort {
	Boolean,
	Byte,
	Short,
	Integer,
	Long,
	UByte,
	UShort,
	UInteger,
	ULong,
	Float,
	Double,
	Char,
	/** A fraction in (0, 1] */
	Fraction,
	/** A fraction in [0, 1] */
	ZFraction,
	/** A rational */
	Rational,
	Void,
	String,
	Class,
	Resource,
	Cell,
	Sequence,
	Set,
	Bag,
	Array,
	Location,
	Process,
	Pointer,
	CVarArgs,
	Option,
	Map,
	Tuple
}
