// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.stmt.decl;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.util.ScalaHelper;
import scala.collection.Iterable;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.util.ASTMapping;
import vct.col.ast.util.ASTMapping1;
import vct.col.ast.util.ASTUtils;
import vct.col.ast.util.ASTVisitor;

import java.util.HashSet;

import static hre.lang.System.Debug;

public class Contract extends ASTNode {

    public final static ASTNode default_true;

    static {
        Origin origin = new MessageOrigin("contract default true");
        default_true = new ConstantExpression(true, origin);
    }

    public final ASTNode invariant;
    public final ASTNode kernelInvariant;
    public final ASTNode pre_condition;
    public final ASTNode post_condition;
    public final DeclarationStatement given[];
    public final DeclarationStatement yields[];
    public final SignalsClause[] signals;
    public final ASTNode modifies[];
    public final ASTNode accesses[];
    private HashSet<String> labels = new HashSet<String>();

    public Contract(
            DeclarationStatement given[],
            DeclarationStatement yields[],
            ASTNode inv,
            ASTNode pre_condition,
            ASTNode post_condition) {
        this.invariant = inv;
        this.kernelInvariant = default_true;
        this.pre_condition = pre_condition;
        this.post_condition = post_condition;
        this.given = given;
        this.yields = yields;
        this.signals = new SignalsClause[0];
        modifies = null;
        accesses = null;
        build_labels();
    }

    public Contract(
            DeclarationStatement given[],
            DeclarationStatement yields[],
            ASTNode modifies[],
            ASTNode accesses[],
            ASTNode inv,
            ASTNode pre_condition,
            ASTNode post_condition) {
        this.invariant = inv;
        this.kernelInvariant = default_true;
        this.pre_condition = pre_condition;
        this.post_condition = post_condition;
        this.given = given;
        this.yields = yields;
        this.modifies = modifies;
        this.accesses = accesses;
        this.signals = new SignalsClause[0];
        build_labels();
    }

    public Contract(
            DeclarationStatement given[],
            DeclarationStatement yields[],
            ASTNode modifies[],
            ASTNode accesses[],
            ASTNode inv,
            ASTNode kernelInvariant,
            ASTNode pre_condition,
            ASTNode post_condition,
            SignalsClause[] signals) {
        this.invariant = inv;
        this.kernelInvariant = kernelInvariant;
        this.pre_condition = pre_condition;
        this.post_condition = post_condition;
        this.given = given;
        this.yields = yields;
        this.modifies = modifies;
        this.accesses = accesses;
        this.signals = signals;
        build_labels();
    }

    @Override
    public <R, A> R accept_simple(ASTMapping1<R, A> map, A arg) {
        return map.map(this, arg);
    }

    public boolean isEmpty() {
        return invariant.isConstant(default_true)
                && pre_condition.isConstant(default_true)
                && post_condition.isConstant(default_true)
                && given.length == 0 && yields.length == 0
                && signals.length == 0
                && modifies == null
                ;
    }

    public void build_labels() {
        for (ASTNode part : ASTUtils.conjuncts(pre_condition, StandardOperator.Star)) {
            for (NameExpression lbl : part.getLabels()) {
                labels.add(lbl.getName());
            }
        }
        for (ASTNode part : ASTUtils.conjuncts(post_condition, StandardOperator.Star)) {
            for (NameExpression lbl : part.getLabels()) {
                labels.add(lbl.getName());
            }
        }
    }

    public boolean hasLabel(String name) {
        return labels.contains(name);
    }


    @Override
    public <T> void accept_simple(ASTVisitor<T> visitor) {
        try {
            visitor.visit(this);
        } catch (Throwable t) {
            if (thrown.get() != t) {
                Debug("Triggered by %s:", getOrigin());
                thrown.set(t);
            }
            throw t;
        }
    }

    @Override
    public <T> T accept_simple(ASTMapping<T> map) {
        try {
            return map.map(this);
        } catch (Throwable t) {
            if (thrown.get() != t) {
                Debug("Triggered by %s:", getOrigin());
                thrown.set(t);
            }
            throw t;
        }
    }

    @Override
    public Iterable<String> debugTreeChildrenFields() {
        return ScalaHelper.toIterable("invariant", "pre_condition", "post_condition", "given", "yields", "signals", "modifies", "accesses");
    }

    @Override
    public Iterable<String> debugTreePropertyFields() {
        return ScalaHelper.toIterable();
    }
}

