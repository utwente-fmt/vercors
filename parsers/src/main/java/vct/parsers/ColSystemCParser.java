package vct.parsers;

import de.tub.pes.syscir.sc_model.SCSystem;
import org.antlr.v4.runtime.CharStream;
import vct.parsers.transform.BlameProvider;
import vct.parsers.transform.OriginProvider;
import vct.parsers.transform.systemctocol.engine.Transformer;
import vct.parsers.transform.systemctocol.colmodel.COLSystem;
import vct.result.VerificationError;

public class ColSystemCParser extends Parser {
    public ColSystemCParser(OriginProvider originProvider, BlameProvider blameProvider) {
        super(originProvider, blameProvider);
    }

    @Override
    public <G> ParseResult<G> parse(CharStream stream) {
        throw new VerificationError.Unreachable("Should not call SystemC parser with ANTLR CharStream!");
    }

    public <G> ParseResult<G> parse(Readable readable) {
        SCSystem sc_system = null;  // TODO

        Transformer<G> sc_to_col_transformer = new Transformer<>(sc_system);
        sc_to_col_transformer.create_col_model();
        COLSystem<G> col_system = sc_to_col_transformer.get_col_system();

        return col_system.to_parse_result();
    }
}
