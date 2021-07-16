package vct.col.ast.expr;

/**
 * The possible kinds of defined names
 */
public enum NameExpressionKind {
    /**
     * an unresolved name
     */
    Unresolved,
    /**
     * argument to a function
     */
    Argument,
    /**
     * local variable
     */
    Local,
    /**
     * a field in a class
     */
    Field,
    /**
     * a method in a class
     */
    Method,
    /**
     * for the reserved names: null, this, and super.
     */
    Reserved,
    /**
     * for labels, such as statement labels and predicate labels.
     */
    Label,
    /**
     * for the ?x binder of VeriFast.
     */
    Output,
    /**
     * name of an ADT
     */
    ADT;
}
