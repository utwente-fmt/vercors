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
	/** A non-zero fraction */
	Fraction,
	/** A zero-able fraction */
	ZFraction,
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
	Option
}
