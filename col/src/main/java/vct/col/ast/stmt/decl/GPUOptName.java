package vct.col.ast.stmt.decl;

public enum GPUOptName {
    LoopUnroll("loop_unroll"), MatrixLinearization("matlin");

    private String name;

    GPUOptName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}
