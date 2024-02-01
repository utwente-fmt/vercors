package vct.parsers.transform.systemctocol.invariants;

import vct.col.ast.*;

import java.util.ArrayList;
import java.util.List;

public class AbstractProcess<T> {

    protected RunMethod<T> process_method;

    public AbstractProcess(RunMethod<T> method) {
        this.process_method = method;
    }

    public List<AbstractState<T>> simulate(int[] program_counter) {
        return null;
    }

    private List<ExecutionState<T>> small_step() {
        return null;
    }

    /**
     * Returns the current statement stack at the execution state given by the program counter. Iteratively resolves the
     * context of each statement and returns a stack with the highest-level container at position 0 and the statement
     * currently to be evaluated at position n - 1.
     *
     * @param program_counter A list of indices showing which point in each container points to the right statement
     * @return A list of container statements, with the desired statement in the last position
     */
    private List<Statement<T>> resolve(int[] program_counter) {
        List<Statement<T>> stack = new ArrayList<>();
        Statement<T> context = process_method.body().get();
        stack.add(context);
        for(int index : program_counter) {
            context = get_at_index(context, index);
            stack.add(context);
        }
        return stack;
    }

    /**
     * Takes a statement that (can) contain other statements as well as an index and returns the given index statement
     * contained within the container.
     *
     * @param container Container statement
     * @param index Index of desired contained statement
     * @return Statement at index <code>index</code> within the body of <code>container</code>
     */
    private Statement<T> get_at_index(Statement<T> container, int index) {
        // Ignores the following:
        //   TryCatchFinally
        //   Synchronized
        //   ParInvariant
        //   ParAtomic
        //   VecBlock
        //   WandPackage
        //   ModelDo
        if (container instanceof PVLBranch<T> pvl_branch) {
            return pvl_branch.branches().apply(index)._2();
        }
        else if (container instanceof PVLLoop<T> pvl_loop) {
            return switch (index) {
                case 0 -> pvl_loop.init();
                case 1 -> pvl_loop.body();
                case 2 -> pvl_loop.update();
                default -> throw new IndexOutOfBoundsException("Loop index must at most be 2.");
            };
        }
        else if (container instanceof InvokeProcedure<T> invoke_procedure) {
            if (index == 0) {
                return invoke_procedure.ref().decl().body().get();
            }
            else throw new IndexOutOfBoundsException("Invalid index for procedure invocation.");
        }
        else if (container instanceof InvokeConstructor<T> invoke_constructor) {
            if (index == 0) {
                return invoke_constructor.ref().decl().body().get();
            }
            else throw new IndexOutOfBoundsException("Invalid index for constructor invocation.");
        }
        else if (container instanceof InvokeMethod<T> invoke_method) {
            if (index == 0) {
                return invoke_method.ref().decl().body().get();
            }
            else throw new IndexOutOfBoundsException("Invalid index for method invocation.");
        }
        else if (container instanceof Block<T> block) {
            return block.statements().apply(index);
        }
        else if (container instanceof Scope<T> scope) {
            if (index == 0) {
                return scope.body();
            }
            else throw new IndexOutOfBoundsException("Invalid index for scope.");
        }
        else if (container instanceof Branch<T> branch) {
            return branch.branches().apply(index)._2();
        }
        else if (container instanceof IndetBranch<T> indet_branch) {
            return indet_branch.branches().apply(index);
        }
        else if (container instanceof Switch<T> switch_stmt) {
            if (index == 0) {
                return switch_stmt.body();
            }
            else throw new IndexOutOfBoundsException("Invalid index for switch statement.");
        }
        else if (container instanceof Loop<T> loop) {
            return switch (index) {
                case 0 -> loop.init();
                case 1 -> loop.body();
                case 2 -> loop.update();
                default -> throw new IndexOutOfBoundsException("Loop index must at most be 2.");
            };
        }
        else if (container instanceof RangedFor<T> ranged_for) {
            if (index == 0) {
                return ranged_for.body();
            }
            else throw new IndexOutOfBoundsException("Invalid index for foreach loop.");
        }
        else throw new IllegalArgumentException("Statement " + container + " is not supported as a container.");
    }
}