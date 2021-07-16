package vct.col.rewrite;

import hre.ast.MessageOrigin;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ASTFactory;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;

import java.util.HashSet;

public class AddTypeADT extends AbstractRewriter {

    public static final String ADT_NAME = "TYPE";

    public static final String DIRECT_SUPERCLASS = "directSuperclass";
    public static final String TYPE_OF = "type_of";

    private AxiomaticDataType adt;
    private Method instanceofMethod;
    private HashSet<ClassType> typeConstructors = new HashSet<>();

    public AddTypeADT(ProgramUnit source) {
        super(source);
        create.enter();
        create.setOrigin(new MessageOrigin("Generated type system ADT"));
        adt = create.adt(ADT_NAME);
        adt.add_map(create.function_decl(
                create.class_type(ADT_NAME),
                null,
                DIRECT_SUPERCLASS,
                new DeclarationStatement[]{
                        create.field_decl("t", create.class_type(ADT_NAME))
                },
                null
        ));
        adt.add_map(create.function_decl(
                create.class_type(ADT_NAME),
                null,
                TYPE_OF,
                new DeclarationStatement[]{
                        create.field_decl("val", create.class_type("Ref"))
                },
                null
        ));

        ContractBuilder cb = new ContractBuilder();
        cb.ensures(create.expression(StandardOperator.EQ,
                create.reserved_name(ASTReserved.Result),
                create.expression(StandardOperator.Or,
                        create.expression(StandardOperator.EQ, create.local_name("t"), create.local_name("u")),
                        create.expression(StandardOperator.EQ,
                                create.domain_call(ADT_NAME, DIRECT_SUPERCLASS, create.local_name("t")),
                                create.local_name("u")
                        )
                )
        ));

        instanceofMethod = create.function_decl(
                create.primitive_type(PrimitiveSort.Boolean),
                cb.getContract(),
                "instanceof",
                new DeclarationStatement[]{
                        create.field_decl("t", create.class_type(ADT_NAME)),
                        create.field_decl("u", create.class_type(ADT_NAME))
                },
                null
        );
        instanceofMethod.setFlag(ASTFlags.STATIC, true);

        create.leave();
    }

    /**
     * Encodes an instanceof operation as encoded by the AddTypeADT phase. Intended to be reused if any instanceof
     * checks need to be added later on.
     *
     * @param create ASTFactory used at the call site
     * @param expr   The expression the typeof operator will be applied to. Will be copied into the resulting expression
     * @param type   Currently assumed to be a class type.
     * @return Condition that is true if expr instanceof type holds
     */
    public static ASTNode exprInstanceof(ASTFactory<?> create, AbstractRewriter copyRw, ASTNode expr, ClassType type) {
        return create.expression(StandardOperator.And,
                create.expression(StandardOperator.NEQ,
                        copyRw.rewrite(expr),
                        create.reserved_name(ASTReserved.Null)
                ),
                create.invokation(null, null, "instanceof",
                        create.invokation(create.class_type(ADT_NAME), null, TYPE_OF, copyRw.rewrite(expr)),
                        create.invokation(create.class_type(ADT_NAME), null, "class_" + type
                        )
                ));
    }

    @Override
    public ProgramUnit rewriteAll() {
        ProgramUnit result = super.rewriteAll();
        result.add(adt);
        result.add(instanceofMethod);
        return result;
    }

    @Override
    public void visit(Method m) {
        if (m.getKind() == Method.Kind.Constructor) {
            ASTClass cls = (ASTClass) m.getParent();
            currentContractBuilder = new ContractBuilder();
            currentContractBuilder.ensures(create.expression(StandardOperator.EQ,
                    create.invokation(create.class_type(ADT_NAME), null, TYPE_OF, create.reserved_name(ASTReserved.Result)),
                    create.invokation(create.class_type(ADT_NAME), null, "class_" + cls.name())
            ));
        }
        super.visit(m);
        if (m.getKind() == Method.Kind.Constructor) {
            Method c = (Method) result;
            if (c != null && c.getBody() != null) {
                ASTClass cls = (ASTClass) m.getParent();
                ((BlockStatement) c.getBody()).prepend(create.special(ASTSpecial.Kind.Inhale, create.expression(StandardOperator.EQ,
                        create.invokation(create.class_type(ADT_NAME), null, TYPE_OF, create.reserved_name(ASTReserved.This)),
                        create.invokation(create.class_type(ADT_NAME), null, "class_" + cls.name())
                )));
            }
            result = c;
        }
    }

    public void visit(ASTClass cl) {
        super.visit(cl);
        ASTClass res = (ASTClass) result;

        ensureTypeConstructor(new ClassType(cl.getFullName()));
        // Assume classes extend Object by default
        if (cl.super_classes.length == 0) {
            addDirectSuperclassAxiom(new ClassType(cl.getName()), new ClassType(ClassType.javaLangObjectName()));
        } else if (cl.super_classes.length == 1) {
            // And otherwise a class can only extend one class
            addDirectSuperclassAxiom(new ClassType(cl.getName()), cl.super_classes[0]);
        } else {
            // TODO
            Abort("Cannot extend more than one class or extend something else than Object");
        }
        result = res;
    }

    private String ensureTypeConstructor(ClassType cl) {
        String name = "class_" + cl.getFullName();

        if (!typeConstructors.contains(cl)) {
            adt.add_unique_cons(create.function_decl(
                    create.class_type(ADT_NAME),
                    null,
                    name,
                    new DeclarationStatement[0],
                    null
            ));
            typeConstructors.add(cl);
        }

        return name;
    }

    private void addDirectSuperclassAxiom(ClassType child, ClassType parent) {
        String child_adt_constructor = ensureTypeConstructor(child);
        String parent_adt_constructor = ensureTypeConstructor(parent);
        adt.add_axiom(create.axiom(child.getName() + "_" + DIRECT_SUPERCLASS,
                create.expression(StandardOperator.EQ,
                        create.domain_call(ADT_NAME, DIRECT_SUPERCLASS, create.domain_call(ADT_NAME, child_adt_constructor)),
                        create.domain_call(ADT_NAME, parent_adt_constructor)
                )));
    }

    public void visit(OperatorExpression e) {
        switch (e.operator()) {
            case EQ:
                if (e.arg(0).isa(StandardOperator.TypeOf)
                        && e.arg(1) instanceof ClassType) {
                    result = create.expression(StandardOperator.EQ, rewrite(e.arg(0)),
                            create.invokation(create.class_type(ADT_NAME), null, "class_" + e.arg(1)));
                } else if (e.arg(1).isa(StandardOperator.TypeOf)
                        && e.arg(0) instanceof ClassType) {
                    result = create.expression(StandardOperator.EQ, rewrite(e.arg(1)),
                            create.invokation(create.class_type(ADT_NAME), null, "class_" + e.arg(0)));
                } else {
                    super.visit(e);
                }
                break;
            case Instance:
                result = exprInstanceof(create, copy_rw, rewrite(e.arg(0)), rewrite((ClassType) e.arg(1)));
                break;
            case TypeOf:
                result = create.invokation(create.class_type(ADT_NAME), null, TYPE_OF, rewrite(e.first()));
                break;
            default:
                super.visit(e);
                break;
        }
    }
}
