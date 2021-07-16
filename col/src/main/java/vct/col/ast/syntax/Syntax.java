// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.syntax;

import hre.ast.TrackingOutput;
import hre.lang.HREError;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.print.AbstractPrinter;
import vct.col.ast.stmt.decl.ASTDeclaration;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;

import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.util.*;

/**
 * This class decouples syntax from structure.
 * E.g. by defining a syntax, one can reuse the implementation of expression printing
 * for multiple languages.
 *
 * @author sccblom
 */
public class Syntax {

    public final String language;
    /**
     * Map annotations to keywords.
     */
    private Map<ASTSpecial.Kind, String> annotation_print =
            new EnumMap<ASTSpecial.Kind, String>(ASTSpecial.Kind.class);
    /**
     * Map keywords to annotations.
     * If an annotation can have a variable number of
     * arguments then the keyword must be unique.
     * Annotations with different numbers of arguments can have
     * overloaded keywords.
     */
    private Map<String, Map<Integer, ASTSpecial.Kind>> annotation_parse = new HashMap<String, Map<Integer, ASTSpecial.Kind>>();
    private Map<StandardOperator, Integer> precedence_map = new EnumMap<StandardOperator, Integer>(StandardOperator.class);
    private Map<StandardOperator, String[]> syntax_map = new EnumMap<StandardOperator, String[]>(StandardOperator.class);
    private Map<StandardOperator, String[]> pattern_map = new EnumMap<StandardOperator, String[]>(StandardOperator.class);
    private Map<StandardOperator, Associativity> associativity_map = new EnumMap<StandardOperator, Associativity>(StandardOperator.class);
    private Map<PrimitiveSort, String> type_map = new EnumMap<PrimitiveSort, String>(PrimitiveSort.class);
    private Map<String, StandardOperator> operator_map = new HashMap<String, StandardOperator>();

    ;
    private Map<String, StandardOperator> function_map = new HashMap<String, StandardOperator>();
    private Map<ASTReserved, String> reserved2syntax = new HashMap<ASTReserved, String>();
    private Map<String, ASTReserved> syntax2reserved = new HashMap<String, ASTReserved>();

    public Syntax(String language) {
        this.language = language;
    }

    public void add_annotation(ASTSpecial.Kind kind, String syntax) {
        Map<Integer, ASTSpecial.Kind> parse = annotation_parse.get(syntax);
        int arity = kind.arity();
        if (parse == null) {
            parse = new HashMap<Integer, Kind>();
            annotation_parse.put(syntax, parse);
        } else {
            if (arity < 0) {
                throw new HREError("keyword %s is already mapped", syntax);
            }
        }
        if (arity < 0) {
            arity = -1;
        }
        parse.put(arity, kind);
        annotation_print.put(kind, syntax);
    }

    public String get_annotation(ASTSpecial.Kind kind) {
        return annotation_print.get(kind);
    }

    public boolean is_annotation(String string) {
        return annotation_parse.get(string) != null;
    }

    public ASTSpecial.Kind get_annotation(String string, int argc) {
        Map<Integer, ASTSpecial.Kind> parse = annotation_parse.get(string);
        if (parse == null) return null;
        ASTSpecial.Kind res = parse.get(-1);
        if (res == null) {
            res = parse.get(argc);
        }
        return res;
    }

    /**
     * Get a pattern that can be matched against an ANTLR 4.x parse tree.
     *
     * @param op
     * @return
     */
    public String[] getPattern(StandardOperator op) {
        return pattern_map.get(op);
    }

    /**
     * Convert a string to an operator.
     *
     * @param op String representation of an operator.
     * @return The standard operator the string represents, if it has been defined,
     * and null otherwise.
     */
    public StandardOperator parseOperator(String op) {
        return operator_map.get(op);
    }

    public StandardOperator parseFunction(String op) {
        return function_map.get(op);
    }

    /**
     * Is the standard function op an operator in this language?
     */
    public boolean isOperator(StandardOperator op) {
        return precedence_map.get(op) != null;
    }

    /**
     * What is the precedence of the given operator in this language?
     */
    public int getPrecedence(StandardOperator op) {
        return precedence_map.get(op);
    }

    /**
     * Get the syntax for the given standard function.
     * the syntax of a function is given as an array of strings,
     * whose length is the arity of the functions plus 1.
     * These strings have to be interleaved with the arguments.
     * E.g. the Java syntax of plus is "","+","".
     */
    public String[] getSyntax(StandardOperator op) {
        return syntax_map.get(op);
    }

    /**
     * What is the associativity of the function in this language?
     */
    public Associativity getAssociativity(StandardOperator op) {
        return associativity_map.get(op);
    }

    /**
     * Add a left associative binary infix operator to the language.
     */
    public void addLeftFix(StandardOperator op, final String syntax, int precedence) {
        String full_syntax[] = {"", syntax, ""};
        precedence_map.put(op, precedence);
        syntax_map.put(op, full_syntax);
        operator_map.put(syntax, op);
        associativity_map.put(op, Associativity.Left);
        String pattern[] = {null, syntax, null};
        pattern_map.put(op, pattern);
    }

    /**
     * Add a non-associative binary infix operator to the language.
     */
    public void addInfix(StandardOperator op, final String syntax, int precedence) {
        String full_syntax[] = {"", syntax, ""};
        precedence_map.put(op, precedence);
        syntax_map.put(op, full_syntax);
        operator_map.put(syntax, op);
        associativity_map.put(op, Associativity.None);
        String pattern[] = {null, syntax, null};
        pattern_map.put(op, pattern);
    }

    /**
     * Add a prefix operator to the language.
     */
    public void addPrefix(StandardOperator op, final String syntax, int precedence) {
        String full_syntax[] = {syntax, ""};
        precedence_map.put(op, precedence);
        syntax_map.put(op, full_syntax);
        operator_map.put(syntax, op);
        associativity_map.put(op, Associativity.None);
        String pattern[] = {syntax, null};
        pattern_map.put(op, pattern);
    }

    /**
     * Add a postfix operator to the language.
     */
    public void addPostfix(StandardOperator op, final String syntax, int precedence) {
        String full_syntax[] = {"", syntax};
        precedence_map.put(op, precedence);
        syntax_map.put(op, full_syntax);
        operator_map.put(syntax, op);
        associativity_map.put(op, Associativity.None);
        String pattern[] = {null, syntax};
        pattern_map.put(op, pattern);
    }

    /**
     * Add a non-associative binary infix operator to the language.
     */
    public void addRightFix(StandardOperator op, final String syntax, int precedence) {
        String full_syntax[] = {"", syntax, ""};
        precedence_map.put(op, precedence);
        syntax_map.put(op, full_syntax);
        operator_map.put(syntax, op);
        associativity_map.put(op, Associativity.Right);
        String pattern[] = {null, syntax, null};
        pattern_map.put(op, pattern);
    }

    public void addList(StandardOperator op, final String... full_syntax) {
        syntax_map.put(op, full_syntax);
    }

    /**
     * Add a standard operation that is represented with function call syntax
     */
    public void addFunction(StandardOperator op, final String syntax) {
        int N = op.arity();
        String full_syntax[] = new String[N + 1];
        String pattern[] = new String[2 * N + 2];
        pattern[0] = syntax;
        pattern[1] = "(";
        full_syntax[0] = syntax + "(";
        for (int i = 1; i < N; i++) {
            full_syntax[i] = ",";
            pattern[2 * i + 1] = ",";
        }
        full_syntax[N] = ")";
        pattern[2 * N + 1] = ")";
        syntax_map.put(op, full_syntax);
        pattern_map.put(op, pattern);
        function_map.put(syntax, op);
    }

    /**
     * Add a name for a primitive type
     */
    public void addPrimitiveType(PrimitiveSort sort, String name) {
        type_map.put(sort, name);
    }

    /**
     * Get the name of a primitive type
     */
    public String getPrimitiveType(PrimitiveSort sort) {
        return type_map.get(sort);
    }

    public void addOperator(StandardOperator op, int precedence, final String... full_syntax) {
        precedence_map.put(op, precedence);
        for (int i = 0; i < full_syntax.length; i++) {
            if (full_syntax[i] == null) {
                hre.lang.System.Abort("syntax of %s contain null at position %d", op, i);
            }
        }
        syntax_map.put(op, Arrays.copyOf(full_syntax, full_syntax.length));
        associativity_map.put(op, Associativity.None);
        ArrayList<String> pattern = new ArrayList<String>();
        for (int i = 0; i < full_syntax.length; i++) {
            if (i > 0) {
                pattern.add(null);
            }
            if (full_syntax[i].length() > 0) {
                pattern.add(full_syntax[i]);
            }
        }
        pattern_map.put(op, pattern.toArray(full_syntax));
    }

    public void addReserved(ASTReserved word, String string) {
        reserved2syntax.put(word, string);
        syntax2reserved.put(string, word);
    }

    public boolean is_reserved(String text) {
        return syntax2reserved.containsKey(text);
    }

    public ASTReserved reserved(String text) {
        return syntax2reserved.get(text);
    }

    public String getSyntax(ASTReserved word) {
        return reserved2syntax.get(word);
    }

    public AbstractPrinter print(TrackingOutput out, ASTNode n) {
        throw new HREError("Pretty printing %s is not implemented", language);
    }

    public ByteArrayOutputStream print(ASTNode n) {
        ByteArrayOutputStream res = new ByteArrayOutputStream();
        PrintWriter writer = new PrintWriter(res);
        print(writer, n);
        writer.close();
        return res;
    }

    public ByteArrayOutputStream print(ProgramUnit program) {
        ByteArrayOutputStream res = new ByteArrayOutputStream();
        PrintWriter writer = new PrintWriter(res);
        print(writer, program);
        writer.close();
        return res;
    }

    public void print(PrintWriter out, ASTNode n) {
        print(new TrackingOutput(out, false), n);
    }

    public void print(PrintWriter outs, ProgramUnit program) {
        TrackingOutput out = new TrackingOutput(outs, false);
        print(out, program);
    }

    public AbstractPrinter print(TrackingOutput out, ProgramUnit program) {
        AbstractPrinter p = null;
        for (ASTDeclaration item : program.get()) {
            p = print(out, item);
        }
        return p;
    }

    public static enum Associativity {Left, Right, None}

}

