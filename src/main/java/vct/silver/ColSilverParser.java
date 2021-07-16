package vct.silver;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.lang.HREError;
import org.antlr.v4.runtime.CharStream;
import scala.NotImplementedError;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.parsers.Parser;
import viper.api.ViperAPI;

import java.io.File;
import java.io.InputStream;

public class ColSilverParser extends Parser {
    public static <T, E, S, Decl, DFunc, DAxiom, Program>
    ProgramUnit parseFile(File f) {
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
    }

    @Override
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

}
