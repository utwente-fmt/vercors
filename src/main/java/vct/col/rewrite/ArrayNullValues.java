package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.AbstractRewriter;

/**
 * Rewrite pass that rewrites null values to none where they should be converted. This happens when e.g. null is
 * assigned to an array in Java.
 * It is not very clear in what exact situations null values should be converted, but some subset of the conversion is
 * due to type conversion (https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html, specifically assignment,
 * method invocation and casting). It is also induced by some types of expression (e.g. ==).
 */
public class ArrayNullValues extends AbstractRewriter {
    public ArrayNullValues(ProgramUnit source) {
        super(source);
    }

    @Override
    public void visit(NameExpression exp) {
        if(exp.isReserved(ASTReserved.Null) && exp.getType().isPrimitive(PrimitiveSort.Option)) {
            result = create.expression(StandardOperator.Cast, exp.getType(), create.reserved_name(ASTReserved.OptionNone));
        } else {
            super.visit(exp);
        }
    }

    @Override
    public void visit(OperatorExpression exp) {
        switch(exp.operator()) {
            case UPlus:
            case UMinus:
            case Exp:
            case Plus:
            case Minus:
            case Mult:
            case FloorDiv:
            case Mod:
            case BitAnd:
            case BitOr:
            case BitXor:
            case BitNot:
            case AmbiguousAnd:
            case AmbiguousOr:
            case AmbiguousXor:
            case And:
            case Or:
            case Not:
            case Implies:
            case IFF:
            case LeftShift:
            case RightShift:
            case UnsignedRightShift:
                break;

            case EQ:
            case NEQ:
                if(exp.getType().isPrimitive(PrimitiveSort.Option)) {
                    exp.arg(0).setType(exp.getType());
                    exp.arg(1).setType(exp.getType());
                }
                break;

            case GT:
            case GTE:
            case LT:
            case LTE:
                break;

            case ITE:
                if(exp.getType().isPrimitive(PrimitiveSort.Option)) {
                    exp.arg(1).setType(exp.getType());
                    exp.arg(2).setType(exp.getType());
                }
                break;

            case TypeOf:
            case Instance:
            case Cast:
            case SubType:
            case SuperType:
            case InterSect:
                // Unsupported
                break;

            case Assign:
                if(exp.getType().isPrimitive(PrimitiveSort.Option)) {
                    exp.arg(1).setType(exp.getType());
                }
                break;

            case MulAssign:
            case DivAssign:
            case RemAssign:
            case AddAssign:
            case SubAssign:
            case ShlAssign:
            case ShrAssign:
            case SShrAssign:
            case AndAssign:
            case XorAssign:
            case OrAssign:
            case PreIncr:
            case PreDecr:
            case PostIncr:
            case PostDecr:
                break;

            case Star:
            case Wand:
                break;

            case Perm:
            case Value:
            case HistoryPerm:
            case ActionPerm:
            case ArrayPerm:
                break;

            case PointsTo:
                // Induces a conversion from the type check, but does not propagate it.
                break;

            case StructSelect:
                break;
            case StructDeref:
                break;
            case AddsTo:
                break;
            case Subscript:
                break;

            case Old:
                if(exp.getType().isPrimitive(PrimitiveSort.Option)) {
                    exp.arg(0).setType(exp.getType());
                }
                break;

            case New:
                break;
            case NewSilver:
                break;
            case NewArray:
                break;

            case Length:
                break;
            case Size:
                break;

            case Cons:
            case Drop:
            case Take:
            case Concat:
            case Head:
            case Tail:
                break;

            case Member:
                // Only allowed for sequence, set or bag.
                break;
            case AddrOf:
                break;
            case SizeOf:
                break;

            case BindOutput:
                break;
            case Wrap:
                if(exp.getType().isPrimitive(PrimitiveSort.Option)) {
                    exp.arg(0).setType(exp.getType());
                }
                break;
            case CurrentPerm:
                break;
            case Scale:
                break;
            case RangeSeq:
                break;
            case Unfolding:
                break;
            case LeftMerge:
                break;
            case History:
                break;
            case Future:
                break;
            case AbstractState:
                break;
            case IndependentOf:
                break;
            case Contribution:
                break;
            case ReducibleSum:
                break;
            case ReducibleMax:
                break;
            case ReducibleMin:
                break;
            case Held:
                break;
            case Identity:
                if(exp.getType().isPrimitive(PrimitiveSort.Option)) {
                    exp.arg(0).setType(exp.getType());
                }
                break;
            case Indirection:
                break;
            case IterationOwner:
                break;
            case PVLidleToken:
                break;
            case PVLjoinToken:
                break;
            case OptionSome:
                break;
            case OptionGet:
                break;
            case ValidArray:
            case ValidMatrix:
                // this should probably be type-checked to be a proper Array, not an Option.
                break;
            case Values:
                // this is type-checked to be a proper Array
                break;
            case FoldPlus:
                break;
            case VectorRepeat:
                break;
            case VectorCompare:
                break;
            case MatrixSum:
                break;
            case MatrixRepeat:
                break;
            case MatrixCompare:
                break;
        }

        super.visit(exp);
    }
}
