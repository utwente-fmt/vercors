package vct.col.rewrite;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.NameExpressionKind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;

import java.util.HashSet;
import java.util.Set;

import static vct.col.ast.type.ASTReserved.This;

/**
 * Rewrites a program that uses inheritance into a program that does not
 * use inheritance any more, such that if the result verifies then the
 * original is correct.
 * <p>
 * Known limitations:
 * <UL>
 * <LI> No support for interfaces or multiple inheritance.
 * <LI> (Pure) method contracts with requires and/or ensures formulas that are not a single predicate invokation.
 * <li> no support for closed and/or final methods yet.
 * <li> No automatic inheritance for non-void methods.
 * </UL>
 *
 * @author Stefan Blom
 */
public class DynamicStaticInheritance extends AbstractRewriter {

    private static final String AT_STRING = "_at_";
    private AbstractRewriter copy_abstract = new AbstractRewriter(this) {
        @Override
        public void visit(Method m) {
            result = create.method_kind(m.kind, rewrite(m.getReturnType()), rewrite(m.getContract()), m.getName(), rewrite(m.getArgs()), m.usesVarArgs(), null);
        }
    };
    private AbstractRewriter tag_this = new AbstractRewriter(this) {
        public void visit(MethodInvokation e) {
            if (isThis(e.object())) {
                String class_name = this.current_class().getName();
                result = create.invokation(rewrite(e.object()), rewrite(e.dispatch()), e.method() + AT_STRING + class_name, rewrite(e.getArgs()));
            } else if (isSuper(e.object())) {
                String class_name = this.current_class().super_classes[0].getName();
                result = create.invokation(create.reserved_name(This), rewrite(e.dispatch()), e.method() + AT_STRING + class_name, rewrite(e.getArgs()));
            } else {
                super.visit(e);
            }
        }
    };
    private AbstractRewriter split_predicates = new AbstractRewriter(this) {
        public void visit(MethodInvokation e) {
            if (e.dispatch() != null) {
                result = create.invokation(
                        rewrite(e.object()),
                        null,
                        e.method() + AT_STRING + e.dispatch().getFullName(),
                        rewrite(e.getArgs())
                );
            } else {
                super.visit(e);
            }
        }
    };
    private AbstractRewriter fix_super = new AbstractRewriter(this) {
        public void visit(MethodInvokation e) {
            if (isSuper(e.object())) {
                String class_name = this.current_class().super_classes[0].getName();
                result = create.invokation(create.reserved_name(This), rewrite(e.dispatch()), e.method() + AT_STRING + class_name, rewrite(e.getArgs()));
            } else {
                super.visit(e);
            }
        }

        public void visit(ASTSpecial e) {
            switch (e.kind) {
                case Fold:
                case Unfold:
                    result = split_predicates.rewrite(e);
                    break;
                case Open: {
                    MethodInvokation i = (MethodInvokation) e.getArg(0);
                    MethodInvokation res = create.invokation(
                            rewrite(i.object()),
                            null,
                            "open_" + i.method() + AT_STRING + i.dispatch().getFullName(),
                            rewrite(i.getArgs())
                    );
                    result = res;
                    break;
                }
                default:
                    super.visit(e);
                    break;
            }
        }

        public void visit(ClassType t) {
            //TODO: test for ADT or make ADT into NON-class!
            if (t.toString().equals("TYPE")) {
                super.visit(t);
                return;
            }
            ASTClass cl = source().find(t.getNameFull());
            if (cl == null) {
                Method m = source().find_predicate(t.getNameFull());
                if (m == null) {
                    String name[] = t.getNameFull();
                    String new_name[] = new String[name.length - 1];
                    for (int i = 0; i < new_name.length - 1; i++) {
                        new_name[i] = name[i];
                    }
                    int k = new_name.length - 1;
                    new_name[k] = name[k] + AT_STRING + name[k + 1];
                    result = create.class_type(new_name);
                } else {
                    super.visit(t);
                }
            } else {
                super.visit(t);
            }
        }
    };
    private AbstractRewriter fix_super_plus = new AbstractRewriter(this) {
        public void visit(MethodInvokation e) {
            if (isSuper(e.object())) {
                String class_name = this.current_class().super_classes[0].getName();
                result = create.invokation(create.reserved_name(This), rewrite(e.dispatch()), e.method() + AT_STRING + class_name, rewrite(e.getArgs()));
            } else {
                super.visit(e);
            }
        }

        public void visit(ASTSpecial e) {
            switch (e.kind) {
                case Fold:
                case Unfold:
                    result = split_predicates.rewrite(e);
                    break;
                case Open: {
                    MethodInvokation i = (MethodInvokation) e.getArg(0);
                    MethodInvokation res = create.invokation(
                            rewrite(i.object()),
                            null,
                            "open_" + i.method() + AT_STRING + i.dispatch().getFullName(),
                            rewrite(i.getArgs())
                    );
                    result = res;
                    break;
                }
                default:
                    super.visit(e);
                    break;
            }
        }

        public void visit(ClassType t) {
            ASTClass cl = source().find(t.getNameFull());
            if (cl == null) {
                Method m = source().find_predicate(t.getNameFull());
                if (m == null) {
                    String name[] = t.getNameFull();
                    String new_name[] = new String[name.length - 1];
                    for (int i = 0; i < new_name.length - 1; i++) {
                        new_name[i] = name[i];
                    }
                    int k = new_name.length - 1;
                    new_name[k] = name[k] + AT_STRING + name[k + 1];
                    result = create.class_type(new_name);
                } else {
                    super.visit(t);
                }
            } else {
                super.visit(t);
            }
        }
    };
    private Set<ClassType> super_classes = new HashSet<ClassType>();

    public DynamicStaticInheritance(ProgramUnit source) {
        super(source);
        for (ASTDeclaration d : source.get()) {
            if (d instanceof ASTClass) {
                ASTClass cl = (ASTClass) d;
                for (ClassType t : cl.super_classes) {
                    super_classes.add(t);
                }
            }
        }
    }

    public static boolean isThis(ASTNode n) {
        if (!(n instanceof NameExpression)) return false;
        NameExpression name = (NameExpression) n;
        if (name.getKind() != NameExpressionKind.Reserved) return false;
        return name.reserved() == ASTReserved.This;
    }

    public static boolean isSuper(ASTNode n) {
        if (!(n instanceof NameExpression)) return false;
        NameExpression name = (NameExpression) n;
        if (name.getKind() != NameExpressionKind.Reserved) return false;
        return name.getName().equals("super");
    }

    public void visit(ASTClass cl) {
        if (cl.implemented_classes.length > 0) Fail("no support for interfaces");
        if (cl.super_classes.length == 0) {
            ClassType t = new ClassType(cl.getFullName());
            if (super_classes.contains(t)) {
                Warning("%s is a super class", t);
            } else {
                Warning("%s is a stand alone class", t);
                result = fix_super.rewrite(cl);
                return;
            }
        }
        if (cl.super_classes.length > 1) Fail("no support for multiple inheritance");
        ASTClass parent = null;
        if (cl.super_classes.length == 1) {
            parent = target().find(cl.super_classes[0]);
            if (parent == null) {
                Abort("super class %s could not be found", cl.super_classes[0]);
            }
        }
        String class_name = cl.getName();
        ASTClass res = create.ast_class(class_name, cl.kind, rewrite(cl.parameters), new ClassType[0], new ClassType[0]);
        //should be function, but chalice messes up during printing.
        res.add_dynamic(create.predicate("is_a_" + class_name, null));
        res.add_dynamic(create.predicate("instance_of_" + class_name, null));
        for (DeclarationStatement decl : cl.fields()) {
            res.add(decl.apply(copy_rw));
        }
        if (parent != null) {
            for (DeclarationStatement decl : parent.fields()) {
                res.add(decl.apply(copy_rw));
            }
        }
        if (parent == null) {
            for (Method m : cl.methods()) {
                // no abstract constructors!
                if (m.kind == Method.Kind.Constructor) continue;
                // all other kinds have abstract counterparts.
                res.add(copy_abstract.rewrite(m));
            }
        } else {
            for (Method m : parent.methods()) {
                if (m.kind == Method.Kind.Predicate) {
                    res.add(copy_rw.rewrite(m));
                } else {
                    res.add(copy_abstract.rewrite(m));
                }
                if (m.kind == Method.Kind.Constructor) continue;
                String name = m.getName();
                if (!name.contains(AT_STRING)) {
                    Type types[] = m.getArgType();
                    int N = m.getArity();
                    ASTNode names[] = new ASTNode[N];
                    for (int i = 0; i < N; i++) {
                        names[i] = create.local_name(m.getArgument(i));
                    }
                    if (cl.find(name, null, types, false) == null) {
                        Debug("%s inherits %s", cl.getName(), name);
                        if (name.startsWith("is_a_") || name.startsWith("instance_of_")) continue;
                        if (m.isValidFlag(ASTFlags.FINAL) && m.getFlag(ASTFlags.FINAL)) continue;
                        gen_inherited(res, parent, m, names, class_name);
                    } else {
                        Debug("%s overrides %s", cl.getName(), name);
                    }
                }
            }
        }
        for (Method m : cl.methods()) {
            boolean is_final = m.isValidFlag(ASTFlags.FINAL) && m.getFlag(ASTFlags.FINAL);
            create.enter(m);
            switch (m.kind) {
                case Predicate: {
                    int N = m.getArity();
                    ASTNode names[] = new ASTNode[N];
                    for (int i = 0; i < N; i++) {
                        names[i] = create.local_name(m.getArgument(i));
                    }
                    ContractBuilder cb = new ContractBuilder();
                    cb.requires(create.invokation(null, null, m.getName(), names).labeled("family"));
                    cb.requires(create.invokation(null, null, "is_a_" + class_name).labeled("class_of"));
                    cb.ensures(create.invokation(null, null, m.getName() + AT_STRING + class_name, names).labeled("member"));
                    Method open = create.method_decl(
                            create.primitive_type(PrimitiveSort.Void),
                            cb.getContract(),
                            "open_" + m.getName() + AT_STRING + class_name,
                            rewrite(m.getArgs()),
                            null
                    );
                    open.setStatic(m.isStatic());
                    res.add(open);
                    ASTNode body = tag_this.rewrite(m.getBody());
                    Method local = create.method_kind(
                            m.kind,
                            copy_rw.rewrite(m.getReturnType()),
                            null,
                            m.getName() + AT_STRING + class_name,
                            copy_rw.rewrite(m.getArgs()),
                            m.usesVarArgs(),
                            body);
                    local.setStatic(m.isStatic());
                    res.add(local);
                    break;
                }
                case Pure:
                case Plain: {
                    Contract c = m.getContract();
                    if (parent != null) {
                        Method override = parent.find(m.getName(), null, m.getArgType());
                        if (c != null && override != null) {
                            Fail("alternate contracts are not supported at %s", m.getOrigin());
                        }
                        if (override != null) {
                            c = override.getContract();
                        }
                    }
                    Method local = create.method_kind(
                            m.kind,
                            copy_rw.rewrite(m.getReturnType()),
                            (is_final ? copy_rw : tag_this).rewrite(c),
                            m.getName() + AT_STRING + class_name,
                            copy_rw.rewrite(m.getArgs()),
                            m.usesVarArgs(),
                            (is_final ? fix_super : fix_super_plus).rewrite(m.getBody()));
                    local.setStatic(m.isStatic());
                    res.add(local);
                    break;
                }
                case Constructor: {
                    ContractBuilder cb = new ContractBuilder();
                    Contract c = m.getContract();
                    if (c != null) copy_rw.rewrite(c, cb);
                    cb.ensures(create.invokation(create.reserved_name(This), null, "is_a_" + class_name).labeled("class_of"));
                    Method global = create.method_kind(
                            m.kind,
                            copy_rw.rewrite(m.getReturnType()),
                            cb.getContract(),
                            m.getName(),
                            copy_rw.rewrite(m.getArgs()),
                            m.usesVarArgs(),
                            null);
                    res.add_dynamic(global);
                    Method local = create.method_kind(
                            m.kind,
                            copy_rw.rewrite(m.getReturnType()),
                            tag_this.rewrite(m.getContract()),
                            m.getName() + AT_STRING + class_name,
                            copy_rw.rewrite(m.getArgs()),
                            m.usesVarArgs(),
                            fix_super.rewrite(m.getBody()));
                    res.add_dynamic(local);
                    break;
                }
                default:
                    Fail("missing case: %s", m.kind);
            }
        }
        create.leave();
        result = res;
    }

    private void gen_inherited(ASTClass res, ASTClass parent, Method m, ASTNode names[], String class_name) {
        create.enter(m);
        String name = m.getName();
        Contract c = m.getContract();
        BlockStatement block = create.block();
        ASTNode body = block;
        switch (m.kind) {
            case Pure: {
                block.addStatement(create.special(ASTSpecial.Kind.Unfold, tag_this.rewrite(c.pre_condition)));
                block.addStatement(create.return_statement(create.invokation(
                        create.reserved_name(This), null, name + AT_STRING + parent.getName(), names)));
                break;
            }
            case Plain: {
                Type t = m.getReturnType();
                block.addStatement(create.special(ASTSpecial.Kind.Unfold, tag_this.rewrite(c.pre_condition)));
                if (t.isVoid()) {
                    block.addStatement(create.invokation(
                            create.reserved_name(This), null, name + AT_STRING + parent.getName(), names));
                } else {
                    Abort("unsupported non-void method");
                }
                block.addStatement(create.special(ASTSpecial.Kind.Fold, tag_this.rewrite(c.post_condition)));
                break;
            }
            case Predicate: {
                body = create.invokation(
                        create.reserved_name(This),
                        null,
                        m.getName() + AT_STRING + parent.getName(),
                        names);
                body.labeled("parent");
                break;
            }
            default:
                Abort("missing case: %s", m.kind);
        }
        Method local = create.method_kind(
                m.kind,
                copy_rw.rewrite(m.getReturnType()),
                tag_this.rewrite(c),
                m.getName() + AT_STRING + class_name,
                copy_rw.rewrite(m.getArgs()),
                m.usesVarArgs(),
                body);
        res.add_dynamic(local);
        create.leave();
    }


}
