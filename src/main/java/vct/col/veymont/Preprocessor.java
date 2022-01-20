package vct.col.veymont;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import vct.antlr4.generated.LangPVLLexer;
import vct.antlr4.generated.PVLParser;
import vct.antlr4.generated.PVLParserBaseListener;
import vct.antlr4.generated.PVLParserBaseVisitor;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.syntax.PVLSyntax;
import vct.main.Main;
import vct.parsers.PVLtoCOL;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

public class Preprocessor {

    static class AllPermissions implements BiFunction<Map<String, List<String>>, Boolean, String> {

        @Override
        public String apply(Map<String, List<String>> typedIdentifiers, Boolean identifiersAreHeapLocations) {
            StringBuilder b = new StringBuilder("true");
            for (String type : typedIdentifiers.keySet()) {
                for (String identifier : typedIdentifiers.get(type)) {
                    if (identifiersAreHeapLocations) {
                        b.append(" ** Perm(").append(identifier).append(", 1)");
                    }
                    if (isPrimitiveType(type)) {
                        // nothing to do
                    } else if (isArray1DType(type)) {
                        b.append(" ** ").append(identifier).append(" != null");
                        //b.append(" ** Perm(").append(identifier).append("[*], 1)");
                        b.append(" ** (");
                        b.append("\\forall* int i; ");
                        b.append("0 <= i && i < ").append(identifier).append(".length; ");
                        b.append("Perm(").append(identifier).append("[i], 1)");
                        if (!isPrimitiveArrayType(type)) {
                            b.append(" ** ").append(identifier).append("[i].ownership()");
                        }
                        b.append(")");
                    } else if (isArray2DType(type)) {
                        b.append(" ** ").append(identifier).append(" != null");
                        b.append(" ** (");
                        b.append("\\forall* int i; ");
                        b.append("0 <= i && i < ").append(identifier).append(".length; (");
                        b.append("\\forall* int j; ");
                        b.append("0 <= j && j < ").append(identifier).append("[i].length; ");
                        b.append("Perm(").append(identifier).append("[i][j], 1)");
                        if (!isPrimitiveArrayType(type)) {
                            b.append(" ** ").append(identifier).append("[i][j].ownership()");
                        }
                        b.append(")");
                        b.append(")");
                    } else if (isClassType(type)) {
                        b.append(" ** ").append(identifier).append(".ownership()");
                    } else {
                        throw new RuntimeException();
                    }
                }
            }
            return b.toString();
        }

        boolean isPrimitiveType(String s) {
            return Objects.equals(s, "int") || Objects.equals(s, "boolean");
        }

        boolean isPrimitiveArrayType(String type) {
            return type != null && (type.startsWith("int[") || type.startsWith("boolean["));
        }

        boolean isArray1DType(String type) {
            return type != null && type.endsWith("[]") && type.indexOf("]") == type.lastIndexOf("]");
        }

        boolean isArray2DType(String type) {
            return type != null && type.endsWith("[][]") && type.indexOf("]") == type.lastIndexOf("]") - 2;
        }

        boolean isClassType(String type) {
            return type.matches("[a-zA-Z]*");
        }
    }

    static class Listener extends PVLParserBaseListener {
        private String tempClassName;
        private Map<String, List<String>> tempFields;
        private Map<String, List<String>> tempArgs;
        private List<String> tempIdentifiers;

        @Override
        public void enterClaz0(PVLParser.Claz0Context claz) {
            assert tempClassName == null;
            assert tempFields == null;
            tempClassName = claz.identifier().getText();
            tempFields = new LinkedHashMap<>();
        }

        @Override
        public void exitClaz0(PVLParser.Claz0Context claz) {
            assert tempClassName != null;
            assert tempFields != null;

            String s = "inline resource ownership() = " + new AllPermissions().apply(tempFields, true) + ";";
            ParseTree t = parse(s, (tokens, parser) -> parser.methodDecl());
            claz.children.add(4, t);

            tempClassName = null;
            tempFields = null;
        }

        @Override
        public void enterField0(PVLParser.Field0Context field) {
            assert tempIdentifiers == null;
            tempIdentifiers = new ArrayList<>();
        }

        @Override
        public void exitField0(PVLParser.Field0Context field) {
            assert tempIdentifiers != null;

            String type = field.type().getText();
            if (!tempFields.containsKey(type)) {
                tempFields.put(type, new ArrayList<>());
            }
            tempFields.get(type).addAll(tempIdentifiers);

            tempIdentifiers = null;
        }

        @Override
        public void enterIdentifierList0(PVLParser.IdentifierList0Context identifierList) {
            assert tempIdentifiers != null;
            String identifier = identifierList.identifier().getText();
            tempIdentifiers.add(identifier);
        }

        @Override
        public void enterIdentifierList1(PVLParser.IdentifierList1Context identifierList) {
            assert tempIdentifiers != null;
            String identifier = identifierList.identifier().getText();
            tempIdentifiers.add(identifier);
        }

        @Override
        public void enterConstructor0(PVLParser.Constructor0Context ctx) {
            assert tempArgs == null;
            tempArgs = new LinkedHashMap<>();
        }

        @Override
        public void exitConstructor0(PVLParser.Constructor0Context constructor) {
            assert tempArgs != null;

            String s1 = "requires " + new AllPermissions().apply(tempArgs, false) + ";";
            String s2 = "ensures ownership();";
            ParseTree t1 = parse(s1, (tokens, parser) -> parser.valContractClause());
            ParseTree t2 = parse(s2, (tokens, parser) -> parser.valContractClause());

            PVLParser.Contract0Context contract = (PVLParser.Contract0Context) constructor.contract();
            if (contract.children == null) {
                contract.children = new ArrayList<>();
            }
            contract.children.add(0, t2);
            contract.children.add(0, t1);

            if (Objects.equals("SeqProgram", tempClassName)) {
                PVLParserBaseVisitor<String[]> visitor = new PVLParserBaseVisitor<>() {
                    @Override
                    public String[] visitArgs0(PVLParser.Args0Context args) {
                        String type = args.type().getText();
                        String identifier = args.identifier().getText();
                        return new String[]{type + " " + identifier, identifier};
                    }

                    @Override
                    public String[] visitArgs1(PVLParser.Args1Context args) {
                        String type = args.type().getText();
                        String identifier = args.identifier().getText();
                        String[] rest = visit(args.args());
                        return new String[]{type + " " + identifier + ", " + rest[0], identifier + ", " + rest[1]};
                    }
                };
                String[] args = constructor.args() == null ? new String[]{"", ""} : visitor.visit(constructor.args());

                String s = "";
                s += contract.children.stream()
                        .map(Preprocessor::toText)
                        .filter(x -> x.startsWith("requires"))
                        .collect(Collectors.joining());
                s += "void main(" + args[0] + ") {";
                s += "  SeqProgram program = new SeqProgram(" + args[1] + ");";
                s += "  program.run();";
                s += "}";
                ParseTree t = parse(s, (tokens, parser) -> parser.methodDecl());
                constructor.children.add(0, t);
            }

            tempArgs = null;
        }

        @Override
        public void enterMethodDecl0(PVLParser.MethodDecl0Context methodDecl) {
            assert tempArgs == null;
            tempArgs = new LinkedHashMap<>();
        }

        @Override
        public void exitMethodDecl0(PVLParser.MethodDecl0Context methodDecl) {
            assert tempArgs != null;

            boolean isPure = methodDecl.modifier().stream()
                    .map(RuleContext::getText)
                    .anyMatch(s -> Objects.equals(s, "pure"));

            String s1 = isPure ? "requires ownership();" : "context ownership();";
            String s2 = "requires " + new AllPermissions().apply(tempArgs, false) + ";";
            ParseTree t1 = parse(s1, (tokens, parser) -> parser.valContractClause());
            ParseTree t2 = parse(s2, (tokens, parser) -> parser.valContractClause());

            PVLParser.Contract0Context contract = (PVLParser.Contract0Context) methodDecl.contract();
            if (contract.children == null) {
                contract.children = new ArrayList<>();
            }
            contract.children.add(0, t2);
            contract.children.add(0, t1);

            tempArgs = null;
        }

        @Override
        public void enterArgs0(PVLParser.Args0Context args) {
            assert tempArgs != null;

            String type = args.type().getText();
            if (!tempArgs.containsKey(type)) {
                tempArgs.put(type, new ArrayList<>());
            }

            String identifier = args.identifier().getText();
            tempArgs.get(type).add(identifier);
        }

        @Override
        public void enterArgs1(PVLParser.Args1Context args) {
            assert tempArgs != null;

            String type = args.type().getText();
            if (!tempArgs.containsKey(type)) {
                tempArgs.put(type, new ArrayList<>());
            }

            String identifier = args.identifier().getText();
            tempArgs.get(type).add(identifier);
        }

        @Override
        public void enterStatement9(PVLParser.Statement9Context statement) {
            assert tempClassName != null;

            if (Objects.equals("SeqProgram", tempClassName)) {
                PVLParserBaseVisitor<List<String>> visitor = new AndOrVisitor();
                List<String> conditions = visitor.visit(statement.expr());
                StringBuilder b = new StringBuilder("assert true");
                for (int i = 0; i < conditions.size() - 1; i++) {
                    b.append(" && ");
                    b.append("(").append(conditions.get(i)).append(")");
                    b.append(" == ");
                    b.append("(").append(conditions.get(i + 1)).append(")");
                }
                b.append(";");

                String s = b.toString();
                ParseTree t = parse(s, (tokens, parser) -> parser.valStatement());
                statement.children.add(0, t);
            }
        }

        @Override
        public void enterStatement15(PVLParser.Statement15Context statement) {
            assert tempClassName != null;
            assert tempArgs != null;

            String s;
            ParseTree t;
            List<ParseTree> invariants = statement.invariantList().children;

            s = "loop_invariant ownership() ** " + new AllPermissions().apply(tempArgs, false) + ";";
            t = parse(s, (tokens, parser) -> parser.invariant());
            invariants.add(0, t);

            if (Objects.equals("SeqProgram", tempClassName)) {
                PVLParserBaseVisitor<List<String>> visitor = new AndOrVisitor();
                List<String> conditions = visitor.visit(statement.expr());
                StringBuilder b = new StringBuilder("loop_invariant true");
                for (int i = 0; i < conditions.size() - 1; i++) {
                    b.append(" && ").append(conditions.get(i));
                    b.append(" == ").append(conditions.get(i + 1));
                }
                b.append(";");

                s = b.toString();
                t = parse(s, (tokens, parser) -> parser.invariant());
                invariants.add(invariants.size(), t);
            }
        }

        @Override
        public void enterStatement16(PVLParser.Statement16Context statement) {
            assert tempClassName != null;
            assert tempArgs != null;

            String s;
            ParseTree t;
            List<ParseTree> invariants = statement.invariantList().children;

            s = "loop_invariant ownership() ** " + new AllPermissions().apply(tempArgs, false) + ";";
            t = parse(s, (tokens, parser) -> parser.invariant());
            invariants.add(0, t);
        }

        static class AndOrVisitor extends PVLParserBaseVisitor<List<String>> {
            @Override
            public List<String> visitExpr4(PVLParser.Expr4Context expr) {
                return visit(expr.iteExpr());
            }

            @Override
            public List<String> visitIteExpr1(PVLParser.IteExpr1Context iteExpr) {
                return visit(iteExpr.implicationExpr());
            }

            @Override
            public List<String> visitImplicationExpr2(PVLParser.ImplicationExpr2Context implicationExpr) {
                return visit(implicationExpr.andOrExpr());
            }

            @Override
            public List<String> visitAndOrExpr0(PVLParser.AndOrExpr0Context andOr) {
                List<String> conditions = new ArrayList<>(visit(andOr.andOrExpr()));
                conditions.add(andOr.eqExpr().getText());
                return conditions;
            }

            @Override
            public List<String> visitAndOrExpr3(PVLParser.AndOrExpr3Context andOr) {
                return List.of(andOr.eqExpr().getText());
            }
        }
    }

    public static String toText(ParseTree tree) {
        if (tree.getChildCount() == 0) {
            return tree.getText();
        } else {
            List<String> texts = new ArrayList<>();
            for (int i = 0; i < tree.getChildCount(); i++) {
                ParseTree child = tree.getChild(i);
                String text = toText(child);
                if (!Objects.equals(text, "<EOF>")) {
                    texts.add(text);
                }
            }
            return String.join(" ", texts);
        }
    }

    public static <R> R parse(String s, BiFunction<CommonTokenStream, PVLParser, R> f) {
        CharStream input = CharStreams.fromString(s);
        LangPVLLexer lexer = new LangPVLLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        PVLParser parser = new PVLParser(tokens);
        return f.apply(tokens, parser);
    }

    public static void main(String[] args) {
        String fileName = "/Users/sungshik/Desktop/vercors-sessiontypes/papercav/casestudies/pvl/election-3.pvl";
        System.out.println(fileName);
        new Preprocessor().run(fileName);
    }

    public void run(String fileName) {
        try {
            Path inputPath = Path.of(fileName);
            Path outputPath1 = Path.of(fileName.replaceAll("\\.pvl", "-preprocessed.pvl"));
            Path outputPath2 = Path.of(fileName.replaceAll("\\.pvl", "-preprocessed-pretty.pvl"));
            Path outputPath3 = Path.of(fileName.replaceAll("\\.pvl", ".java"));

            // Note: null fields/arguments/local variables are *not* supported

            String input, output;

            input = Files.readString(inputPath);
            output = parse(input, (tokens, parser) -> {
                PVLParser.Program0Context program = (PVLParser.Program0Context) parser.program();
                ParseTreeWalker.DEFAULT.walk(new Listener(), program);
                return toText(program);
            });

            //System.out.println(output);
            Files.writeString(outputPath1, output);

            input = output;
            output = parse(input, (tokens, parser) -> {
                PVLParser.Program0Context program = (PVLParser.Program0Context) parser.program();
                ProgramUnit pu = PVLtoCOL.convert(program, "-", tokens, parser);
                return PVLSyntax.get().print(pu).toString();
            });

            //System.out.println(output);
            Files.writeString(outputPath2, output);



            String[] args;
/*

            args = new String[] {
//                    "--debug",
//                    "vct.main.Main",
                    "--silicon",
                    "--progress",
                    outputPath2.toString()
            };
            Main.main(args);

*/
            args = new String[] {
//                    "--debug",
//                    "vct.main.Main",
                    "--progress",
                    "--veymont",
                    outputPath3.toString(),
                    outputPath2.toString()
            };
            Main.main(args);




        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}