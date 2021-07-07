package vct.col.ast.util;

import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.Type;
import vct.col.ast.util.ASTFactory;

import java.util.Objects;
import java.util.function.Consumer;

import static hre.lang.System.Fail;

public class SequenceUtils {
    public static class SequenceInfo {
        /**
         * The full type of the sequence, e.g. Option<Array<Cell<Integer>>>>
         */
        private PrimitiveType completeType;

        /**
         * The type starting from the sequence type, e.g. Array<Cell<Integer>>>
         */
        private PrimitiveType sequenceType;

        /**
         * The argument to the sequence type, e.g. Cell<Integer>
         */
        private Type sequenceTypeArgument;

        /**
         * The element type of the array, e.g. Integer
         * The type which `x` should be such that x is assignable to array[i]
         */
        private Type elementType;

        private SequenceInfo(PrimitiveType completeType, PrimitiveType sequenceType, Type sequenceTypeArgument, Type elementType) {
            this.completeType = completeType;
            this.sequenceType = sequenceType;
            this.sequenceTypeArgument = sequenceTypeArgument;
            this.elementType = elementType;
        }

        public PrimitiveType getCompleteType() {
            return completeType;
        }

        public PrimitiveType getSequenceType() {
            return sequenceType;
        }

        public PrimitiveSort getSequenceSort() {
            return sequenceType.sort;
        }

        public Type getSequenceTypeArgument() {
            return sequenceTypeArgument;
        }

        public Type getElementType() {
            return elementType;
        }

        /**
         * @return whether the array is assignable after taking one subscript
         */
        public boolean isAssignable() {
            return isCell();
        }

        public boolean isOpt() {
            return completeType.isPrimitive(PrimitiveSort.Option);
        }

        public boolean isCell() {
            return sequenceTypeArgument.isPrimitive(PrimitiveSort.Cell);
        }
    }

    /**
     * Retrieve info about a sequence-like type
     * @param type the type to check
     * @return info about the sequence-like type, or null when it is not a valid type
     */
    public static SequenceInfo getTypeInfo(Type type) {
        // All supported sequence types are primitive types, or wrapped by primitive types
        if(!(type instanceof PrimitiveType)) return null;
        PrimitiveType completeType = (PrimitiveType) type;
        PrimitiveType sequenceType = completeType;

        // The sequence may be wrapped by an option once, which must again contain a primitive type
        if(sequenceType.isPrimitive(PrimitiveSort.Option)) {
            if(((Type) sequenceType.firstarg()) instanceof PrimitiveType) {
                sequenceType = (PrimitiveType) sequenceType.firstarg();
            } else {
                return null;
            }
        }

        // The sequence must be of type Sequence, Bag or Array
        boolean isSeqLike = (
                sequenceType.isPrimitive(PrimitiveSort.Sequence) ||
                sequenceType.isPrimitive(PrimitiveSort.Bag) ||
                sequenceType.isPrimitive(PrimitiveSort.Array)
        );

        if(!isSeqLike) {
            return null;
        }

        // The argument to the sequence type, either Cell<ElementType> or just ElementType.
        Type sequenceTypeArgument = (Type) sequenceType.firstarg();
        Type elementType = sequenceTypeArgument;

        // If the element type is a cell, the sequence is assignable with a subscript
        if(elementType.isPrimitive(PrimitiveSort.Cell)) {
            elementType = (Type) elementType.firstarg();
        }

        return new SequenceInfo(completeType, sequenceType, sequenceTypeArgument, elementType);
    }

    public static SequenceInfo getTypeInfoOrFail(Type type, String message) {
        SequenceInfo result = getTypeInfo(type);

        Objects.requireNonNull(result, String.format(message, type));

        return result;
    }

    public static SequenceInfo getInfo(ASTNode node) {
        return getTypeInfo(node.getType());
    }

    public static SequenceInfo getInfoOrFail(ASTNode node, String message) {
        SequenceInfo result = getInfo(node);

        Objects.requireNonNull(result, String.format(message, node, node.getType()));

        return result;
    }

    public static SequenceInfo expectSort(ASTNode node, String message, PrimitiveSort sort) {
        SequenceInfo result = getInfoOrFail(node, message);

        if(result.getSequenceSort() != sort) {
            Fail(message, node, node.getType());
        }

        return result;
    }

    public static SequenceInfo expectSortType(Type type, String message, PrimitiveSort sort) {
        SequenceInfo result = getTypeInfoOrFail(type, message);

        if(result.getSequenceSort() != sort) {
            Fail(message, type);
        }

        return result;
    }

    public static SequenceInfo expectArray(ASTNode node, String message) {
        return expectSort(node, message, PrimitiveSort.Array);
    }

    public static SequenceInfo expectArrayType(Type type, String message) {
        return expectSortType(type, message, PrimitiveSort.Array);
    }

    public static SequenceInfo expectSequence(ASTNode node, String message) {
        return expectSort(node, message, PrimitiveSort.Sequence);
    }

    public static SequenceInfo expectSequenceType(Type t, String message) {
        return expectSortType(t, message, PrimitiveSort.Sequence);
    }

    /**
     * Subscript a sequence-like object correctly, depending on the type.
     * @param create The factory to use to create AST nodes
     * @param seq The sequence-like object
     * @param index The index to use
     * @return The new node, or null if some error occurs
     */
    public static ASTNode access(ASTFactory<?> create, ASTNode seq, ASTNode index) {
       return accessUsingType(create, seq.getType(), seq, index);
    }

    public static ASTNode accessUsingType(ASTFactory<?> create, Type sequenceType, ASTNode seq, ASTNode index) {
        // TODO: Rewrite all sequence access to be specific, as above.
        // Currently, StandardOperator.Subscript is treated as the generic sequence subscript before the RewriteArrayRef
        // stage, after which it is rewritten to a specific, correct expression. This should just be the correct
        // expression from the start. For now, this method assumes we are before the RewriteArrayRef stage. A similar
        // statement is true for StandardOperator.Size/Length

        return create.expression(StandardOperator.Subscript, seq, index);
    }

    public static void validSequence(ASTFactory<?> create, Consumer<ASTNode> cb, ASTNode seq) {
        validSequenceUsingType(create, cb, seq.getType(), seq);
    }

    public static void validSequenceUsingType(ASTFactory<?> create, Consumer<ASTNode> cb, Type type, ASTNode seq) {
        SequenceInfo info = getTypeInfo(type);
        Objects.requireNonNull(info, String.format("Expected %s to be of a sequence type", seq));

        if(info.isOpt()) {
            cb.accept(create.expression(StandardOperator.NEQ, seq, create.reserved_name(ASTReserved.OptionNone)));
        }
    }

    public static Type optArrayCell(ASTFactory<?> create, Type elementType) {
        return create.primitive_type(PrimitiveSort.Option, arrayCell(create, elementType));
    }

    public static Type arrayCell(ASTFactory<?> create, Type elementType) {
        return create.primitive_type(PrimitiveSort.Array,
            create.primitive_type(PrimitiveSort.Cell,
                elementType
            )
        );
    }

    public static Type optSeqCell(ASTFactory<?> create, Type elementType) {
        return create.primitive_type(PrimitiveSort.Option, seqCell(create, elementType));
    }

    public static Type seqCell(ASTFactory<?> create, Type elementType) {
        return create.primitive_type(PrimitiveSort.Sequence,
            create.primitive_type(PrimitiveSort.Cell,
                elementType
            )
        );
    }
}
