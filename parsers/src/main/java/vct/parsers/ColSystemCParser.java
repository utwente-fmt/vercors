package vct.parsers;

import de.tub.pes.syscir.engine.Engine;
import de.tub.pes.syscir.engine.Environment;
import de.tub.pes.syscir.sc_model.SCSystem;
import hre.io.Readable;
import org.antlr.v4.runtime.CharStream;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import vct.parsers.transform.BlameProvider;
import vct.parsers.transform.OriginProvider;
import vct.parsers.transform.systemctocol.engine.Transformer;
import vct.parsers.transform.systemctocol.colmodel.COLSystem;
import vct.parsers.transform.systemctocol.exceptions.IllegalOperationException;
import vct.result.VerificationError;

import javax.xml.parsers.DocumentBuilderFactory;

public class ColSystemCParser extends Parser {
    public ColSystemCParser(OriginProvider originProvider, BlameProvider blameProvider) {
        super(originProvider, blameProvider);
    }

    @Override
    public <G> ParseResult<G> parse(CharStream stream) {
        throw new VerificationError.Unreachable("Should not call SystemC parser with ANTLR CharStream!");
    }

    public <G> ParseResult<G> parse(Readable readable) {
        // Read XML document from input
        Document document = readable.read(reader -> {
                try {
                    return DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(reader));
                }
                catch (Throwable any_exception) {
                    return null;
                }
            });

        // Use SystemC Intermediate Representation to parse a SystemC system from the document
        if (document == null) throw new IllegalOperationException("Could not open input XML document.");
        Environment environment = Engine.parseSystem(document);
        SCSystem sc_system = environment.getSystem();

        // Transform SystemC system to COL system
        Transformer<G> sc_to_col_transformer = new Transformer<>(sc_system);
        sc_to_col_transformer.create_col_model();
        COLSystem<G> col_system = sc_to_col_transformer.get_col_system();

        // Transform COL system to parse result
        return col_system.to_parse_result();
    }
}
