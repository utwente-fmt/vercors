package vct.col.ast.stmt.decl;

public enum GPUOptName {
    LoopUnroll("loop_unroll",2 ),
    MatrixLinearization("matlin", 4),
    IterationMerging("iter_merge", 2);

    //The name as defined in the gramamr
    private String name;
    private int arity;

    GPUOptName(String name, int arity) {
        this.name = name;
        this.arity = arity;
    }

    public int getArity() {
        return arity;
    }

    @Override
    public String toString() {
        return name;
    }
}
