package vct.parsers;

import hre.lang.HREExitException;
import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import vct.antlr4.generated.LangPVLLexer;
import vct.antlr4.generated.PVLParser;
import vct.col.ast.stmt.decl.ProgramUnit;

import static hre.lang.System.*;

/**
 * Parse specified code and convert the contents to COL.
 */
public class ColPVLParser extends Parser {
    @Override
    public ProgramUnit parse(CharStream input, String file_name) {
        try {
            TimeKeeper tk = new TimeKeeper();
            ErrorCounter ec = new ErrorCounter(file_name);

            LangPVLLexer lexer = new LangPVLLexer(input);

            lexer.removeErrorListeners();
            lexer.addErrorListener(ec);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            PVLParser parser = new PVLParser(tokens);
            parser.removeErrorListeners();
            parser.addErrorListener(ec);
            PVLParser.ProgramContext tree = parser.program();
            Progress("parsing pass took %dms", tk.show());
            ec.report();
            Debug("parser got: %s", tree.toStringTree(parser));

            ProgramUnit pu = PVLtoCOL.convert(tree, file_name, tokens, parser);
            Progress("AST conversion pass took %dms", tk.show());
            return pu;
        } catch (HREExitException e) {
            throw e;
        } catch (Exception e) {
            DebugException(e);
            Abort("Exception %s while parsing %s", e.getClass(), file_name);
        } catch (Throwable e) {
            DebugException(e);
            Warning("Exception %s while parsing %s", e.getClass(), file_name);
            throw e;
        }
        return null;
    }
}

