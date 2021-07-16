package hre.ast;

import java.nio.file.Path;

public class HREOrigins implements OriginFactory<Origin> {

    @Override
    public Origin message(String fmt, Object... args) {
        return new MessageOrigin(fmt, args);
    }

    @Override
    public Origin file(Path file, int line, int col) {
        return new FileOrigin(file, line, col);
    }

    @Override
    public Origin file(Path file, int ln1, int c1, int ln2, int c2) {
        return new FileOrigin(file, ln1, c1, ln2, c2);
    }

}
