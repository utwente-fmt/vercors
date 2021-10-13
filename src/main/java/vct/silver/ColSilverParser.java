package vct.silver;

import org.antlr.v4.runtime.CharStream;
import scala.NotImplementedError;
import vct.parsers.ParseResult;
import vct.parsers.Parser;
import vct.parsers.transform.BlameProvider;
import vct.parsers.transform.OriginProvider;

public class ColSilverParser extends Parser {
    /*@Override
    public ProgramUnit parse(InputStream stream, String name) {
        throw new NotImplementedError(
                "Cannot parse a silver file from an InputStream: the silver parser requires a file");
    }

    @Override
    public ProgramUnit parse(CharStream stream, String name) {
        throw new NotImplementedError(
                "Cannot parse a silver file from a CharStream: the silver parser requires a file");
    }

    @Override
    public ProgramUnit parse(File file) {
        return parseFile(file);
    }

    public static ProgramUnit parseFile(File f) {
        ViperAPI<Origin, ?, ?, ?, ?, ?, ?> viper =
                SilverBackend.getVerifier("parser");
        return parseFile(f, viper);
    }

    public static <Program> ProgramUnit parseFile(File f, ViperAPI<Origin, ?, ?, ?, ?, ?, Program> viper) {
        Program program = viper.prog.parse_program(f.toString());
        if (program == null) {
            throw new HREError("parsing %s failed", f);
        }
        VerCorsViperAPI vercors = VerCorsViperAPI.get();
        ProgramUnit tmp = viper.prog.convert(vercors, program);
        ProgramUnit res = new ProgramUnit();
        ASTClass ref = new ASTClass("Ref", ASTClass.ClassKind.Record);
        ref.setOrigin(new MessageOrigin("implicit Ref for %s", f));
        res.add(ref);
        for (ASTNode d : tmp) {
            if (d instanceof DeclarationStatement) {
                ref.add_dynamic(d);
            } else {
                res.add(d);
            }
        }
        return res;
    }*/

    @Override
    public ParseResult parse(CharStream stream, OriginProvider originProvider, BlameProvider blameProvider) {
        throw new NotImplementedError(); // FIXME PB: should re-implement sil -> col parsing for new col
    }
}
