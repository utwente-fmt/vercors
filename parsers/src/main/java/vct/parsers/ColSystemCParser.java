package vct.parsers;

import de.tub.pes.syscir.engine.Engine;
import de.tub.pes.syscir.engine.Environment;
import hre.io.Readable;
import de.tub.pes.syscir.sc_model.SCSystem;
import org.antlr.v4.runtime.CharStream;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import vct.parsers.transform.BlameProvider;
import vct.parsers.transform.OriginProvider;
import vct.parsers.transform.systemctocol.engine.Transformer;
import vct.parsers.transform.systemctocol.colmodel.COLSystem;
import vct.parsers.transform.systemctocol.util.OriGen;
import vct.result.VerificationError;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.Reader;

public class ColSystemCParser extends Parser {
    public ColSystemCParser(OriginProvider originProvider, BlameProvider blameProvider) {
        super(originProvider, blameProvider);
    }

    @Override
    public <G> ParseResult<G> parse(CharStream stream) {
        throw new VerificationError.Unreachable("Should not call SystemC parser with ANTLR CharStream!");
    }

    public <G> ParseResult<G> parse(Readable readable) {
        Document document = readable.read((r) -> {
                try {
                    return DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(r));
                }
                catch (Throwable ignored) {return null;}
            });

        Environment environment = Engine.parseSystem(document);

        SCSystem sc_system = environment.getSystem();

        Transformer<G> sc_to_col_transformer = new Transformer<>(sc_system);
        sc_to_col_transformer.create_col_model();
        COLSystem<G> col_system = sc_to_col_transformer.get_col_system();

        return col_system.to_parse_result();
    }
}
