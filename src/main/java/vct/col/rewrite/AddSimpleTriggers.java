package vct.col.rewrite;

import vct.col.ast.expr.BindingExpression;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.ASTUtils;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.RecursiveVisitor;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.concurrent.atomic.AtomicBoolean;

public class AddSimpleTriggers extends AbstractRewriter {

    public AddSimpleTriggers(ProgramUnit pu) {
        super(pu);
    }

    /**
     * Illigal (trigger) expressions are those with arithmetic operators or
     * accessibility predicates (Perm operator).
     *
     * @param clause
     * @return
     */
    public static boolean isIlligalTriggerExpression(ASTNode clause) {
        final AtomicBoolean res = new AtomicBoolean(false);
        RecursiveVisitor<Boolean> scanner = new RecursiveVisitor<Boolean>((ProgramUnit) null) {
            public void visit(OperatorExpression e) {
                boolean invalid = true;
                // assume every operation is illigal except ...
                switch (e.operator()) {
                    case Subscript:
                    case OptionGet:
                    case Length:
                    case Size:
                        invalid = false;
                        break;
                    default:
                        break;
                }
                if (invalid) {
                    res.set(true);
                } else {
                    super.visit(e);
                }
            }

            public void visit(BindingExpression e) {
                res.set(true);
            }
        };
        clause.accept(scanner);
        return res.get();
    }

    @Override
    public void visit(BindingExpression e) {
        switch (e.binder()) {
            case Forall:
            case Star:
                if (e.javaTriggers() == null || e.javaTriggers().length == 0) {
                    ASTNode main = rewrite(e.main());
                    ASTNode triggers[][] = getTriggers(e.getDeclarations(), and(e.select(), main));
                    result = create.binder(e.binder(), e.result_type(), e.getDeclarations(), triggers, e.select(), main);
                } else {
                    super.visit(e);
                }
                break;
            default:
                super.visit(e);
                break;
        }
    }

    private ASTNode[][] getTriggers(DeclarationStatement[] declarations, ASTNode main) {
        ArrayList<ASTNode[]> triggerSets = new ArrayList<ASTNode[]>();
        HashSet<ASTNode> res = new HashSet<ASTNode>();
        collectTriggersFor(declarations, main, res);
        Debug("Found " + res.size() + " trigger kandidates");
        ASTNode[] nodes = res.toArray(new ASTNode[res.size()]);
        HashSet<HashSet<ASTNode>> sets = powerSet(0, nodes);
        sets = validSets(sets, declarations);
        for (HashSet<ASTNode> set : sets) {
            triggerSets.add(set.toArray(new ASTNode[set.size()]));
        }
        if (triggerSets.isEmpty()) {
            return null;
        } else {
            Debug("Found triggers: " + printTriggerSets(triggerSets));
            return triggerSets.toArray(new ASTNode[][]{});
        }
    }

    private HashSet<HashSet<ASTNode>> powerSet(int i, ASTNode[] nodes) {
        HashSet<HashSet<ASTNode>> res = new HashSet<HashSet<ASTNode>>();
        if (i < nodes.length) {
            for (HashSet<ASTNode> set : powerSet(i + 1, nodes)) {
                res.add(set);
                HashSet<ASTNode> withSet = new HashSet<ASTNode>();
                withSet.addAll(set);
                withSet.add(nodes[i]);
                res.add(withSet);
            }
            HashSet<ASTNode> newSet = new HashSet<ASTNode>();
            newSet.add(nodes[i]);
            res.add(newSet);
        }
        return res;
    }

    private HashSet<HashSet<ASTNode>> validSets(HashSet<HashSet<ASTNode>> sets, DeclarationStatement[] declarations) {
        HashSet<HashSet<ASTNode>> res = new HashSet<HashSet<ASTNode>>();
        for (HashSet<ASTNode> set : sets) {
            if (isValidTriggerSetFor(set, declarations)) {
                res.add(set);
            }
        }
        return res;
    }

    private HashSet<HashSet<ASTNode>> minimumSets(HashSet<HashSet<ASTNode>> sets, ASTNode[] nodes) {
        HashSet<HashSet<ASTNode>> res = new HashSet<HashSet<ASTNode>>();
        for (ASTNode trigger : nodes) {
            HashSet<ASTNode> minSet = null;
            for (HashSet<ASTNode> set : sets) {
                if (set.contains(trigger)) {
                    if (minSet == null || set.size() < minSet.size()) {
                        minSet = set;
                    }
                }
            }
            res.add(minSet);
        }
        return res;
    }

    private HashSet<HashSet<ASTNode>> maximumSets(HashSet<HashSet<ASTNode>> sets, ASTNode[] nodes) {
        HashSet<HashSet<ASTNode>> res = new HashSet<HashSet<ASTNode>>();
        for (ASTNode trigger : nodes) {
            HashSet<ASTNode> maxTriggerSet = null;
            for (HashSet<ASTNode> set : sets) {
                if (set.contains(trigger)) {
                    if (maxTriggerSet == null || set.containsAll(maxTriggerSet)) {
                        maxTriggerSet = set;
                    }
                }
            }
            res.add(maxTriggerSet);
        }
        return res;
    }

    private void collectTriggersFor(DeclarationStatement[] decls, ASTNode main, HashSet<ASTNode> triggers) {
        RecursiveVisitor<Boolean> scanner = new RecursiveVisitor<Boolean>((ProgramUnit) null) {
            public void visit(OperatorExpression e) {
                if (validTriggerFor(decls, e)) {
                    triggers.add(e);
                } else {
                    super.visit(e);
                }
            }

            public void visit(MethodInvokation e) {
                if (validTriggerFor(decls, e)) {
                    triggers.add(e);
                } else {
                    super.visit(e);
                }
            }

            public void visit(BindingExpression e) {
                return;
            }
        };
        main.accept(scanner);
    }

    /**
     * Check if all declarations are covered by the triggers.
     *
     * @param triggers
     * @param declarations
     * @return True if all declarations occur at least once in the set of triggers.
     * False otherwise.
     */
    private boolean isValidTriggerSetFor(HashSet<ASTNode> triggers, DeclarationStatement[] declarations) {
        boolean res = declarations != null && declarations.length > 0;
        for (DeclarationStatement decl : declarations) {
            boolean hasDecl = false;
            for (ASTNode trigger : triggers) {
                hasDecl |= ASTUtils.find_name(trigger, decl.name());
            }
            res &= hasDecl;
        }
        return res;
    }

    /**
     * Check if node is a valid trigger for one or more of the declarations.
     *
     * @param decls
     * @param node
     * @return
     */
    private boolean validTriggerFor(DeclarationStatement[] decls, ASTNode node) {
        boolean res = false;
        for (DeclarationStatement decl : decls) {
            if (node.isName(decl.name())) {
                // Node has no structure. A trigger can not be the quantified variable itself.
                res = false;
                break;
            } else {
                // Trigger should contain at least one of the quantified variables.
                res |= ASTUtils.find_name(node, decl.name());
            }
        }
        // Trigger can not have arithmetic operators or accessibility predicates.
        return res && !isIlligalTriggerExpression(node);
    }

    private String printTriggerSets(ArrayList<ASTNode[]> triggerSets) {
        String s = "";
        for (ASTNode[] triggerSet : triggerSets) {
            s += String.format("{%s}", printTriggerSet(triggerSet));
        }
        return s;
    }

    private String printTriggerSet(ASTNode[] triggerSet) {
        String s = "";
        if (triggerSet.length > 0) {
            s += triggerSet[0];
            for (int i = 1; i < triggerSet.length; i++) {
                s += ", " + triggerSet[i];
            }
        }
        return s;
    }

}
