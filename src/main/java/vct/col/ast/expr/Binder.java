package vct.col.ast.expr;

public enum Binder {
	Lambda,
	Forall,
	Exists,
	Sum,
	Product,
	Star,
	Let,

	/**
	 * The Max binder is used in expressions like (\max int i; i \memberof {a..b}; c). Currently it is not mapped to any
	 * frontend, but only used in rewrite rules. Special care is taken in the rewrite rules to make sure \max only
	 * appears in conditions where the range is non-empty, because otherwise its value would be undefined, whereas we
	 * simplify (\max i \in {a..b} . i) to b-1.
	 */
	Max,
	Min
}
