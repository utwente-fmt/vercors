package vct.col.ast.print;

import hre.ast.TrackingOutput;
import hre.lang.HREError;
import hre.util.LambdaHelper;
import org.apache.commons.lang3.StringEscapeUtils;
import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StringValue;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.BeforeAfterAnnotations;
import vct.col.ast.langspecific.c.*;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.syntax.PVLSyntax;
import vct.col.ast.type.*;
import vct.col.ast.util.ASTUtils;
import vct.col.ast.util.ClassName;
import vct.col.ast.util.SequenceUtils;

import java.util.*;


/*
Allows printing PVL programs from ASTNode
 */
public class PVLPrinter extends AbstractPrinter{

    public PVLPrinter(TrackingOutput out) {
        super(PVLSyntax.get(),out);
    }

    /**
     * Flag set before visiting loop invariants.
     */
    private boolean loopcontract = false;


    public void visit(TypeVariable v){
        out.print(v.name());
    }

    public void pre_visit(ASTNode node){
        super.pre_visit(node);
        for(NameExpression lbl:node.getLabels()){
            nextExpr();
            lbl.accept(this);
            out.printf(":");
        }
        if (node.annotated()) for(ASTNode ann:node.annotations()) {
            if (ann==null){
                out.printf(" <null annotation> ");
            } else {
                if(!(node instanceof Method && ((Method)node).kind == Method.Kind.Pure)) {
                    nextExpr();
                    ann.accept(this);
                    out.printf(" ");
                }
            }
        }
    }

    @Override
    public void visit(TryCatchBlock tcb){
        out.print("try");
        tcb.main().accept(this);
        for (CatchClause cb : tcb.catchesJava()) {
            cb.accept(this);
        }
        if (tcb.after() != null){
            out.print(" finally ");
            tcb.after().accept(this);
        }
        out.println("");
    }

    @Override
    public void visit(NameSpace ns){
        if (!ns.name().equals(NameSpace.NONAME)) {
            out.printf("package %s;",ns.getDeclName().toString("."));
            out.println("");
        } else {
            out.println("// begin of package");
        }
        for(NameSpace.Import i:ns.imports){
            out.printf("import %s%s",i.static_import?"static ":"",new ClassName(i.name).toString("."));
            if(i.all){
                out.println(".*;");
            } else {
                out.println(";");
            }
        }
        for(ASTNode n:ns){
            n.accept(this);
        }
        out.println("// end of package");
    }

    @Override
    public void visit(ActionBlock ab){
        out.printf("action(");
        nextExpr(); ab.history().accept(this);
        out.printf(",");
        nextExpr(); ab.fraction().accept(this);
        out.printf(",");
        nextExpr(); ab.process().accept(this);
        out.printf(",");
        nextExpr(); ab.action().accept(this);

        // visit all (key/value) entries in `ab.map` (via a lambda)
        ab.foreach(LambdaHelper.fun((key, val) -> {
            out.printf(", %s, ", key);
            nextExpr();
            val.accept(this);
        }));

        out.printf(")");
        ab.block().accept(this);
    }

    public void post_visit(ASTNode node){
        if (node instanceof BeforeAfterAnnotations && !(node instanceof LoopStatement)){
            BeforeAfterAnnotations baa=(BeforeAfterAnnotations)node;
            if (baa.get_before()!=null && baa.get_before().size()>0 || baa.get_after()!=null && baa.get_after().size()>0){
                BlockStatement tmp=baa.get_before();
                if (tmp!=null && tmp.size()>0) {
                    out.printf("with ");
                    tmp.accept(this);
                }
                tmp=baa.get_after();
                if (tmp!=null && tmp.size()>0) {
                    out.printf("then ");
                    tmp.accept(this);
                }
            }
        }
        super.post_visit(node);
    }

    @Override
    public void visit(Axiom axiom){
        out.printf("axioms %s: ", axiom.name());
        axiom.rule().accept(this);
        out.println(";");
    }

    @Override
    public void visit(AxiomaticDataType adt){
        out.printf("ADT %s [", adt.name());
        String sep="";
        for (DeclarationStatement d : adt.parametersJava()) {
            out.printf("%s%s", sep, d.name());
            sep=", ";
        }
        out.println("] {");
        out.incrIndent();
        out.println("//constructors");
        for (Method f : adt.constructorsJava()) {
            f.accept(this);
        }
        out.println("//mappings");
        for(Method f:adt.mappingsJava()){
            f.accept(this);
        }
        out.println("//axioms");
        for(Axiom ax:adt.axiomsJava()){
            ax.accept(this);
        }
        out.decrIndent();
        out.println("}");
    }
    @Override
    public void visit(ASTSpecial s){
        String syn=syntax.get_annotation(s.kind);
        if (syn!=null){
            out.print(syn);
            setExpr();
            String sep=" ";
            for(ASTNode n:s.args){
                out.print(sep);
                sep=",";
                n.accept(this);
            }
            out.println(";");
            return;
        }
        ASTSpecial e=s;
        switch(s.kind){
            case Refute:{
                out.printf("refute ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case Assume:{
                out.printf("assume ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case HoarePredicate:{
                out.printf("/*{ ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                out.lnprintf(" }*/");
                break;
            }
            case Unfold:{
                out.printf("unfold ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case Fold:{
                out.printf("fold ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case Use:{
                out.printf("use ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case Witness:{
                out.printf("witness ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case Apply:{
                out.printf("apply ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case QED:{
                out.printf("qed ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case Open:{
                out.printf("open ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case Close:{
                out.printf("close ");
                current_precedence=0;
                setExpr();
                ASTNode prop=e.getArg(0);
                prop.accept(this);
                break;
            }
            case Fork:{
                out.printf("fork ");
                current_precedence=0;
                setExpr();
                ASTNode prop=s.args[0];
                prop.accept(this);
                out.println(";");
                break;
            }
            case Join:{
                out.printf("join ");
                current_precedence=0;
                setExpr();
                ASTNode prop=s.args[0];
                prop.accept(this);
                out.println(";");
                break;
            }
            case Goto:
                out.print("goto ");
                s.args[0].accept(this);
                break;
            case Label:
                out.print("label ");
                s.args[0].accept(this);
                break;
            case With:
                out.print("WITH");
                s.args[0].accept(this);
                break;
            case Then:
                out.print("THEN");
                s.args[0].accept(this);
                break;
            case Expression:
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case Assert:
                out.print("assert ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case Lock:
                out.print("lock ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case Unlock:
                out.print("unlock ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case Wait:
                out.print("wait ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case Notify:
                out.print("notify ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case Import:
                out.print("import ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case Throw:
                out.print("throw ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case Exhale:
                out.print("exhale ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case Inhale:
                out.print("inhale ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case CreateHistory:
                out.print("create ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case CreateFuture:
                out.print("create ");
                setExpr();
                s.args[0].accept(this);
                out.printf(",");
                s.args[1].accept(this);
                out.println(";");
                break;
            case DestroyHistory:
                out.print("destroy ");
                setExpr();
                s.args[0].accept(this);
                out.printf(",");
                s.args[1].accept(this);
                out.println(";");
                break;
            case DestroyFuture:
                out.print("destroy ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case SplitHistory:
                out.print("split ");
                setExpr();
                s.args[0].accept(this);
                out.printf(",");
                s.args[1].accept(this);
                out.printf(",");
                s.args[2].accept(this);
                out.printf(",");
                s.args[3].accept(this);
                out.printf(",");
                s.args[4].accept(this);
                out.println(";");
                break;
            case MergeHistory:
                out.print("merge ");
                setExpr();
                s.args[0].accept(this);
                out.printf(",");
                s.args[1].accept(this);
                out.printf(",");
                s.args[2].accept(this);
                out.printf(",");
                s.args[3].accept(this);
                out.printf(",");
                s.args[4].accept(this);
                out.println(";");
                break;
            case CSLSubject:
                out.print("csl_subject ");
                setExpr();
                s.args[0].accept(this);
                out.println(";");
                break;
            case SpecIgnoreStart:
                out.println("spec_ignore {");
                break;
            case SpecIgnoreEnd:
                out.println("} spec_ignore");
                break;
            default:
                super.visit(s);
                break;
        }
    }

    @Override
    public void visit(ClassType t){
        out.print(t.getFullName());
        if (t.hasArguments()) {
            setExpr();
            out.print("<");
            ASTNode args[] = t.argsJava().toArray(new ASTNode[0]);
            if (args.length>=1) {
                args[0].accept(this);
                for(int i = 1; i < args.length; i++){
                    out.print(",");
                    args[i].accept(this);
                }
            }
            out.print(">");
        }
    }

    public void visit(FunctionType t){
        Type[] types = t.paramsJava().toArray(new Type[0]);

        for (int i=0;i<types.length-1;i++) {
            types[i].accept(this);
            out.print(",");
        }
        types[types.length].accept(this);

        out.print("->");
        t.result().accept(this);
    }

    public void visit(BindingExpression e){
        String binder=null;
        switch(e.binder()){
            case Forall:
                binder="\\forall";
                break;
            case Exists:
                binder="\\exists";
                break;
            case Star:
                binder="\\forall*";
                break;
            case Let:
                binder="\\let";
                break;
            case Sum:
                binder="\\sum";
                break;
            case SetComp:
                //TODO Have a correct way of outputting this.
                out.printf("setcomp");
                break;
            default:
                Abort("binder %s unimplemented",e.binder());
        }
        setExpr();
        out.printf("(%s ",binder);
        int N=e.getDeclCount();
        for(int i=0;i<N;i++){
            if (i>0) out.printf(",");
            DeclarationStatement decl=e.getDeclaration(i);
            decl.getType().accept(this);
            out.printf(" %s", decl.name());
            if (decl.initJava() != null) {
                out.printf("= ");
                decl.initJava().accept(this);
            }
        }
        if (e.triggers()!=null){
            for(ASTNode trigger[]:e.javaTriggers()){
                out.printf("{");
                trigger[0].accept(this);
                for(int i=1;i<trigger.length;i++){
                    out.printf(",");
                    trigger[i].accept(this);
                }
                out.printf("}");
            }
        }
        out.printf(";");
        if (e.select()!=null){
            e.select().accept(this);
            out.printf(";");
        }
        e.main().accept(this);
        out.printf(")");
    }

    public void visit(BlockStatement block){
        out.lnprintf("{");
        out.incrIndent();
        int N=block.getLength();
        for(int i=0;i<N;i++){
            ASTNode statement=block.getStatement(i);
            statement.accept(this);
            if (self_terminating(statement)){
                out.clearline();
            } else {
                out.lnprintf(";");
            }
        }
        out.decrIndent();
        out.printf("}");
    }

    @Override
    public void visit(ASTClass cl){
        visit(cl.getContract());
        if(cl.kind == ASTClass.ClassKind.Plain) {
            out.printf("class %s",cl.getName());
        } else {
            Abort("unexpected class kind %s",cl.kind);
        }
        out.lnprintf(" {");
        out.incrIndent();
        boolean declStatRead = true;
        for(ASTNode item:cl){
            if(!(item instanceof DeclarationStatement) && declStatRead) {//put newline after class fields
                declStatRead = false;
                out.println("");
            }
            if (item.isStatic()){
                if (item instanceof DeclarationStatement) out.printf("static ");
            }
            item.accept(this);
            out.println("");
        }
        out.decrIndent();
        out.lnprintf("}");
        out.println("");
    }

    @Override
    public void visit(Contract contract) {
        if (contract!=null){
            //out.incrIndent();
            for (DeclarationStatement d:contract.given){
                out.printf("given ");
                d.accept(this);
                out.lnprintf("");
            }
            for(ASTNode e:ASTUtils.conjuncts(contract.invariant,StandardOperator.Star, StandardOperator.And, StandardOperator.Wrap)){
                out.printf((loopcontract) ? "loop_invariant ": "context_everywhere ");
                nextExpr();
                e.accept(this);
                out.lnprintf(";");
            }
            for(ASTNode e: ASTUtils.conjuncts(contract.pre_condition,StandardOperator.Star)){
                out.printf("requires ");
                nextExpr();
                e.accept(this);
                out.lnprintf(";");
            }
            for (DeclarationStatement d:contract.yields){
                out.printf("yields ");
                d.accept(this);
                out.lnprintf("");
            }
            for(ASTNode e:ASTUtils.conjuncts(contract.post_condition,StandardOperator.Star)){
                out.printf("ensures ");
                nextExpr();
                e.accept(this);
                out.lnprintf(";");
            }
            for (SignalsClause sc : contract.signals){
                sc.accept(this);
            }
            if (contract.modifies!=null){
                out.printf("modifies ");
                if (contract.modifies.length==0){
                    out.lnprintf("\\nothing;");
                } else {
                    nextExpr();
                    contract.modifies[0].accept(this);
                    for(int i=1;i<contract.modifies.length;i++){
                        out.printf(", ");
                        nextExpr();
                        contract.modifies[i].accept(this);
                    }
                    out.lnprintf(";");
                }
            }
            if (contract.accesses!=null){
                out.printf("accessible ");
                if (contract.accesses.length==0){
                    out.lnprintf("\\nothing;");
                } else {
                    nextExpr();
                    contract.accesses[0].accept(this);
                    for(int i=1;i<contract.accesses.length;i++){
                        out.printf(", ");
                        nextExpr();
                        contract.accesses[i].accept(this);
                    }
                    out.lnprintf(";");
                }
            }
            //out.decrIndent();
        }
    }

    public void visit(SignalsClause sc) {
        out.printf("signals (");
        sc.type().accept(this);
        out.printf(" %s) ",sc.name());
        nextExpr();
        sc.condition().accept(this);
        out.lnprintf(";");
    }

    public void visit(DeclarationStatement s){
        ASTNode expr = s.initJava();
        nextExpr();
        s.getType().accept(this);
        out.printf(" %s", s.name());
        if (expr!=null){
            out.printf(" = ");
            nextExpr();
            expr.accept(this);
        }
        if (!in_expr) out.printf(";");
    }

    public void visit(Method m){
        int N=m.getArity();
        Type result_type=m.getReturnType();
        String name=m.getName();
        Contract contract=m.getContract();
        boolean predicate=m.getKind()==Method.Kind.Predicate;
        if (predicate && contract!=null){
                Debug("ignoring contract of predicate");
        }
        if (!m.getGpuOpts().isEmpty()) {
            m.getGpuOpts().forEach(opt -> visit(opt));
        }

        if (contract!=null && !predicate){
            visit(contract);
        }
        for(int i=1;i<0xF000;i<<=1){
            if (m.isValidFlag(i)){
                if (m.getFlag(i)){
                    switch(i){
                        case ASTFlags.STATIC:
                        case ASTFlags.FINAL:
                            break;
                        case ASTFlags.INLINE:
                            out.printf("inline ");
                        case ASTFlags.PUBLIC:
                            //no public in PVL
                            break;
                        case ASTFlags.THREAD_LOCAL:
                            out.printf("thread_local  ");
                            break;
                        case ASTFlags.EXTERN:
                            out.printf("extern ");
                            break;
                        case ASTFlags.UNIQUE:
                            out.printf("unique ");
                            break;
                        default:
                            throw new HREError("unknown flag %04x",i);
                    }
                }
            }
        }
        if (!m.isValidFlag(ASTFlags.STATIC)) {
            out.printf("static?? ");
        } else if (m.isStatic()) out.printf("static ");
        if (m.isValidFlag(ASTFlags.FINAL) && m.getFlag(ASTFlags.FINAL)){
            out.printf("final ");
        }
        if (m.getKind()==Method.Kind.Pure){
            out.printf("pure ");
        }
        if (m.getKind()!=Method.Kind.Constructor){
            result_type.accept(this);
            out.printf(" ");
        }
        out.printf("%s(",name);
        if (N>0) {
            DeclarationStatement args[]=m.getArgs();
            if (args[0].isValidFlag(ASTNode.GHOST) && args[0].isGhost()){ out.printf("ghost "); }
            if (args[0].isValidFlag(ASTFlags.OUT_ARG) && args[0].getFlag(ASTFlags.OUT_ARG)){ out.printf("out "); }
            m.getArgType(0).accept(this);
            if (N==1 && m.usesVarArgs()){
                out.print(" ...");
            }
            out.printf(" %s",m.getArgument(0));
            for(int i=1;i<N;i++){
                out.printf(",");
                if (args[i].isValidFlag(ASTNode.GHOST) && args[i].isGhost()){ out.printf("ghost"); }
                if (args[i].isValidFlag(ASTFlags.OUT_ARG) && args[i].getFlag(ASTFlags.OUT_ARG)){ out.printf("out "); }
                m.getArgType(i).accept(this);
                if (i==N-1 && m.usesVarArgs()){
                    out.print(" ...");
                }
                out.printf(" %s",m.getArgument(i));
            }
        }
        out.printf(")");
        if (m.signals.length > 0) {
            out.printf(" throws ");
            m.signals[0].accept(this);
            if (m.signals.length > 1) {
                for (int i = 1; i < m.signals.length; i++) {
                    Type t = m.signals[i];
                    out.printf(", ");
                    t.accept(this);
                }
            }
        }
        ASTNode body=m.getBody();
        if (body==null) {
            out.lnprintf(";");
        } else if (body instanceof BlockStatement) {
            body.accept(this);
            out.lnprintf("");
        } else {
            out.printf(" = ");
            nextExpr();
            body.accept(this);
            out.lnprintf(";");
        }
    }

    public void visit(IfStatement s){
        int N=s.getCount();
        out.printf("if (");
        nextExpr();
        s.getGuard(0).accept(this);
        out.print(") ");
        s.getStatement(0).accept(this);
        if (!self_terminating(s.getStatement(0))){
            out.printf(";");
        }
        for(int i=1;i<N;i++){
            if (self_terminating(s.getStatement(i-1))){
                out.printf(" ");
            }
            if (i==N-1 && s.getGuard(i)==IfStatement.elseGuard()){
                out.printf("else ");
            } else {
                out.printf(" else if (");
                nextExpr();
                s.getGuard(i).accept(this);
                out.lnprintf(") ");
            }
            s.getStatement(i).accept(this);
            if (!self_terminating(s.getStatement(i))){
                out.lnprintf(";");
            }
        }
    }

    private boolean self_terminating(ASTNode s) {
        return (s instanceof BlockStatement)
                || (s instanceof IfStatement)
                || (s instanceof LoopStatement)
                || (s instanceof ASTSpecial)
                || (s instanceof DeclarationStatement)
                || (s instanceof ParallelRegion)
                || (s instanceof ParallelBarrier)
                || (s instanceof ParallelAtomic)
                || (s instanceof ParallelInvariant)
                ;
    }

    public void visit(AssignmentStatement s){
        setExpr();
        s.location().accept(this);
        out.printf(" = ");
        s.expression().accept(this);
    }

    public void visit(ReturnStatement s){
        ASTNode expr=s.getExpression();
        if (expr==null){
            out.lnprintf("return");
        } else {
            out.printf("return ");
            setExpr();
            expr.accept(this);
        }
        if (s.get_after()!=null && s.get_after().size() > 0){
            out.printf("then ");
            s.get_after().accept(this);
        }
    }

    public void visit(Lemma lemma){
        out.printf("lemma ");
        lemma.block().accept(this);
    }

    public void visit(OperatorExpression e){
        if (e.isa(StandardOperator.NewArray)) {
            String[] op_syntax =syntax.getSyntax(e.operator());

            out.print(op_syntax[0]);

            SequenceUtils.SequenceInfo info = SequenceUtils.getTypeInfo((Type) e.arg(0));
            while (info != null && !info.isCell()) {
                info = SequenceUtils.getTypeInfo(info.getSequenceTypeArgument());
            }
            if (info == null || info.getElementType() == null) {
                super.visit(e);
                return;
            }
            info.getElementType().accept(this);


            for(int i=1;i<e.args().size();i++){
                out.print(op_syntax[1]);
                boolean tmp = in_expr;
                in_expr = true;
                e.arg(i).accept(this);
                in_expr = tmp;
                out.print(op_syntax[2]);
            }
        } else {
            visitVerCors(e);
        }

    }

    private void visitVerCors(OperatorExpression e){
        switch(e.operator()){
            case NewSilver:{
                out.print("new ");
                // no break on purpose!
            }
            case Wrap:{
                out.print("(");
                String sep="";
                for (ASTNode arg : e.argsJava()) {
                    out.print(sep);
                    sep=",";
                    arg.accept(this);
                }
                out.print(")");
                break;
            }
            case New:{
                out.printf("new ");
                e.arg(0).accept(this);
                out.printf("()");
                break;
            }
            case NewArray:{
                out.printf("new ");
                if(e.arg(0) instanceof PrimitiveType) {
                    PrimitiveType p = ((PrimitiveType)e.arg(0));
                    if(p.sort == PrimitiveSort.Option && p.args().head() instanceof PrimitiveType) {
                        PrimitiveType p2 = (PrimitiveType) p.args().head();
                        if(p2.hasArguments()) {
                            p2.args().head().accept(this);
                        }
                    } else {
                        e.arg(0).accept(this);
                    }
                } else {
                    e.arg(0).accept(this);
                }
                out.print("[");
                e.arg(1).accept(this);
                out.printf("]");
                break;
            }
            default:{
                super.visit(e);
            }
        }
    }

    @Override
    public void visit(TypeExpression t){
        switch (t.operator()) {
            case Extends:
                out.printf("? extends ");
                t.firstType().accept(this);
                return;
            case Super:
                out.printf("? super ");
                t.firstType().accept(this);
                return;
            default:
                super.visit(t);
        }
    }

    @Override
    public void visit(Switch s){
        out.printf("switch (");
        nextExpr();
        s.expr.accept(this);
        out.println("){");
        for(Switch.Case c:s.cases){
            for(ASTNode n:c.cases){
                out.printf("case ");
                nextExpr();
                n.accept(this);
                out.println(":");
            }
            out.incrIndent();
            for(ASTNode n:c.stats){
                n.accept(this);
                out.println("");
            }
            out.decrIndent();
        }
        out.println("}");
    }

    @Override
    public void visit(StructValue v) {
        setExpr();
        if (v.type() != null) {
            v.type().accept(this);
        }
        out.print("{");
        String sep="";
        for (int i = 0; i < v.valuesLength(); i++) {
            out.print(sep);
            sep=",";
            v.value(i).accept(this);
        }
        out.print("}");
    }

    public void visit(ForEachLoop s){
        visit(s.getContract());
        out.printf("for(");
        for(DeclarationStatement decl:s.decls){
            nextExpr();
            decl.apply(this);
            out.printf(";");
        }
        nextExpr();
        s.guard.apply(this);
        out.printf(")");
        s.body.apply(this);
    }

    private void visitForStatementList(BlockStatement s) {
        boolean first = true;
        for(ASTNode n : s){
            if(!first) {
                out.printf(", ");
            }

            nextExpr();
            n.accept(this);

            first = false;
        }
    }

    public void visit(GPUOpt o) {
        if (o == null) return;
        out.printf("gpuopt ");
        if (o instanceof LoopUnrolling) {
            out.printf("loop_unroll ");
        } else if (o instanceof MatrixLinearization) {
            out.printf("matrix_lin ");
        } else if (o instanceof Tiling) {
            out.printf("tile ");
        } else if (o instanceof IterationMerging) {
            out.printf("iter_merge ");
        } else if (o instanceof DataLocation) {
            out.printf("glob_to_reg ");
        }
        else {
            Warning("Could not find name of " + o.getClass());
        }
        if (o instanceof MatrixLinearization) {
            nextExpr();
            ((MatrixLinearization) o).matrixName().accept(this);
            out.print(" ");
            nextExpr();
            out.print(((MatrixLinearization) o).rowOrColumn().equals(Major.Row()) ?"R":"C");
            out.print(" ");
            nextExpr();
            ((MatrixLinearization) o).dimX().accept(this);
            out.print(" ");
            nextExpr();
            ((MatrixLinearization) o).dimY().accept(this);
        } else if (o instanceof Tiling) {
            nextExpr();
            out.print(((Tiling) o).interOrIntra().equals(TilingConfig.Inter()) ?"inter":"intra");
            out.print(" ");
            nextExpr();
            ((Tiling) o).tileSize().accept(this);
        } else {
            Iterator<ASTNode> argsit = o.argsJava().iterator();
            print_tuple(" ", "", "", o.argsJava().toArray(new ASTNode[0]));
        }
        out.lnprintf(";");
    }

    public void visit(LoopStatement s){
        visit(s.getGpuopt());
        loopcontract = true;
        visit(s.getContract());
        loopcontract = false;
        ASTNode tmp;
        if (s.getInitBlock()!=null || s.getUpdateBlock()!=null){
            out.printf("for(");

            if(s.getInitBlock() != null) {
                if (s.getInitBlock() instanceof BlockStatement) {
                    visitForStatementList((BlockStatement) s.getInitBlock());
                } else {
                    nextExpr();
                    s.getInitBlock().accept(this);
                }
            }
            out.printf(";");

            nextExpr();
            s.getEntryGuard().accept(this);
            out.printf(";");

            if((s.getUpdateBlock())!=null) {
                if(s.getUpdateBlock() instanceof BlockStatement){
                    visitForStatementList((BlockStatement) s.getUpdateBlock());
                } else {
                    nextExpr();
                    s.getUpdateBlock().accept(this);
                }
            }
            out.printf(")");
        } else if ((tmp=s.getEntryGuard())!=null) {
            out.printf("while(");
            nextExpr();
            tmp.accept(this);
            out.printf(")");
        } else {
            out.printf("do");
        }
        if (s.get_before()!=null && s.get_before().size()>0 || s.get_after()!=null  && s.get_after().size()>0){
            out.println("");
            out.incrIndent();
        }
        if (s.get_before()!=null && s.get_before().size()>0){
            out.printf("with ");
            s.get_before().accept(this);
            out.println("");
        }
        if (s.get_after()!=null  && s.get_after().size()>0){
            out.printf("then ");
            s.get_after().accept(this);
            out.println("");
        }
        if (s.get_before()!=null && s.get_before().size()>0 || s.get_after()!=null  && s.get_after().size()>0){
            out.decrIndent();
        }
        tmp=s.getBody();
        if (!(tmp instanceof BlockStatement)) { out.printf(" "); }
        tmp.accept(this);
        tmp=s.getExitGuard();
        if (tmp!=null){
            out.printf("while(");
            nextExpr();
            tmp.accept(this);
            out.lnprintf(")");
        }
    }

        private void print_tuple(ASTNode ... args) {
            print_tuple(",", "(", ")", args);
        }

        private void print_tuple(String delimiter, String prefix, String suffix, ASTNode ... args) {
        out.print(prefix);
        String sep="";
        for(ASTNode n:args){
            out.print(sep);
            nextExpr();
            n.accept(this);
            sep=delimiter;
        }
        out.print(suffix);
    }

    public void visit(MethodInvokation s){
        if (s.method().equals(Method.JavaConstructor)){
            setExpr();
            out.print("new ");
            s.dispatch().accept(this);
            print_tuple(s.getArgs());
        } else {
            super.visit(s);
        }
    }

    public void visit(Dereference e){
        e.obj().accept(this);
        out.printf(".%s", e.field());
    }

    public void visit(PrimitiveType t){
        int nrofargs = t.nrOfArguments();

        switch(t.sort){
            case Pointer:{
                t.firstarg().accept(this);
                out.printf("*");
                break;
            }
            case Array:
                SequenceUtils.SequenceInfo info = SequenceUtils.getTypeInfo(t);
                if (info != null && info.isCell()) {
                    info.getElementType().accept(this);
                } else {
                    t.firstarg().accept(this);
                }
                switch(nrofargs){
                    case 1:
                        out.printf("[]");
                        return;
                    case 2:
                        out.printf("[/*");
                        t.secondarg().accept(this);
                        out.printf("*/]");
                        return;
                    default:
                        Fail("Array type constructor with %d arguments instead of 1 or 2",nrofargs);
                }
            case Cell:
                if (nrofargs==2){
                    out.printf("cell<");
                    t.firstarg().accept(this);
                    out.printf(">[");
                    t.secondarg().accept(this);
                    out.printf("]");
                    break;
                }
                if (nrofargs!=1){
                    Fail("Cell type constructor with %d arguments instead of 1",nrofargs);
                }
                t.firstarg().accept(this);
                break;
            case Option:
                if (nrofargs!=1){
                    Fail("Option type constructor with %d arguments instead of 1",nrofargs);
                }

                SequenceUtils.SequenceInfo info1 = SequenceUtils.getTypeInfo(t);
                if (info1 != null && info1.getSequenceSort() == PrimitiveSort.Array){

                    info1.getElementType().apply(this);
                    out.printf("[]");
                    break;
                } else {
                    out.printf("option<");
                    t.firstarg().accept(this);
                    out.printf(">");
                }

                break;
            case Map:
                if (nrofargs!=2){
                    Fail("Map type constructor with %d arguments instead of 2",nrofargs);
                }
                out.printf("map<");
                t.firstarg().accept(this);
                out.printf(",");
                t.secondarg().accept(this);
                out.printf(">");
                break;
            case Tuple:
                if (nrofargs!=2){
                    Fail("Tuple type constructor with %d arguments instead of 2",nrofargs);
                }
                out.printf("tuple<");
                t.firstarg().accept(this);
                out.printf(",");
                t.secondarg().accept(this);
                out.printf(">");
                break;
            case Sequence:
                if (nrofargs!=1){
                    Fail("Sequence type constructor with %d arguments instead of 1",nrofargs);
                }
                out.printf("seq<");
                t.firstarg().accept(this);
                out.printf(">");
                break;
            case Set:
                if (nrofargs!=1){
                    Fail("Set type constructor with %d arguments instead of 1",nrofargs);
                }
                out.printf("set<");
                t.firstarg().accept(this);
                out.printf(">");
                break;
            case Bag:
                if (nrofargs!=1){
                    Fail("Bag type constructor with %d arguments instead of 1",nrofargs);
                }
                out.printf("bag<");
                t.firstarg().accept(this);
                out.printf(">");
                break;
            case CVarArgs:
                out.printf("...");
                break;
            default:
                super.visit(t);
        }
    }

    @Override
    public void visit(ParallelAtomic pa){
        out.printf("atomic (");
        String sep="";

        for (ASTNode item : pa.synclistJava()) {
            out.printf("%s", sep);
            sep = ",";
            nextExpr();
            item.apply(this);
        }

        out.printf(")");
        pa.block().accept(this);
    }

    @Override
    public void visit(ParallelBlock pb){

        int j = 0;
        out.printf(pb.label());
        out.printf("(");
        for (DeclarationStatement iter : pb.itersJava()) {
            if (j > 0) out.printf(",");
            in_expr = true;
            iter.accept(this);
            in_expr = false;

            j++;
        }
        out.printf(")");
        out.newline();

        if (pb.depslength() > 0){
            out.printf(";");
            pb.dependency(0).accept(this);
            for (int i = 1; i < pb.depslength(); i++) {
                out.printf(",");
                pb.dependency(i).accept(this);
            }
        }
        if (pb.contract() != null) {
            visit(pb.contract());
        }
        pb.block().accept(this);
    }

    @Override
    public void visit(ParallelBarrier pb){
        if (pb.contract() == null) {
            Fail("parallel barrier with null contract!");
        } else {
            out.printf("barrier(%s)", pb.label());
            if (pb.body() == null) {
                out.println(" { ");
                out.incrIndent();
                visit(pb.contract());
                out.decrIndent();
                out.println("}");
            } else {
                out.newline();
                out.incrIndent();
                visit(pb.contract());
                out.decrIndent();
                pb.body().accept(this);
            }
        }
    }
    @Override
    public void visit(ParallelInvariant pb) {
        out.printf("invariant %s (", pb.label());
        nextExpr();
        pb.inv().accept(this);
        out.printf(")");
        pb.block().accept(this);
    }
    @Override
    public void visit(ParallelRegion region){
        out.print("par ");

        if (region.contract() != null) {
            region.contract().accept(this);
        }
        for (Iterator<ParallelBlock> it = region.blocksJava().iterator(); it.hasNext();) {
            out.incrIndent();
            it.next().accept(this);
            if(it.hasNext()) {
                out.print(" and");
            }
            out.println("");
            out.decrIndent();
        }
    }

    public void visit(ConstantExpression ce){
        if (ce.value() instanceof StringValue){
            out.print("\""+ StringEscapeUtils.escapeJava(ce.toString())+"\"");
        } else {
            out.print(ce.toString());
        }
    }

    @Override
    public void visit(VariableDeclaration decl){
        decl.basetype.accept(this);
        String sep=" ";
        for(ASTDeclaration dd:decl.get()){
            out.print(sep);
            sep=",";
            if (dd instanceof DeclarationStatement){
                DeclarationStatement d = (DeclarationStatement)dd;
                out.print(d.name());
                ASTNode init = d.initJava();
                if (init!=null){
                    out.print(" = ");
                    setExpr();
                    init.accept(this);
                }
            } else {
                out.print("TODO");
            }
        }
    }

    @Override
    public void visit(FieldAccess a) {
        setExpr();
        if (a.value() == null) {
            out.printf("((");
        }
        if (a.object() != null) {
            a.object().apply(this);
            out.printf(".");
        }
        out.printf("%s", a.name());
        if (a.value() != null) {
            out.printf(" := ");
            a.value().apply(this);
        } else {
            out.printf("))");
        }
    }

    @Override
    public void visit(VectorBlock v){
        out.print("vec(");
        nextExpr();
        v.iter().accept(this);
        out.println(")");
        v.block().apply(this);
    }

    @Override
    public void visit(Constraining c){
        out.print("constraining(");
        String sep = "";

        for (NameExpression n : c.varsJava()) {
            out.print(sep);
            nextExpr();
            n.accept(this);
            sep=",";
        }

        out.print(")");
        c.block().accept(this);
    }

    private void visitNames(List<String> names) {
        boolean first = true;
        for(String name : names) {
            if(!first) out.print(", ");
            first = false;
            out.print(name);
        }
    }

    private void visitOmpOptions(List<OMPOption> options) {
        for(OMPOption option : options) {
            out.print(" ");
            if(option instanceof OMPNoWait$) {
                out.print("nowait");
            } else if(option instanceof OMPPrivate) {
                out.print("private(");
                visitNames(((OMPPrivate) option).namesJava());
                out.print(")");
            } else if(option instanceof OMPShared) {
                out.print("shared(");
                visitNames(((OMPShared) option).namesJava());
                out.print(")");
            } else if(option instanceof OMPSimdLen) {
                out.printf("simdlen(%d)", ((OMPSimdLen) option).len());
            } else if(option instanceof OMPNumThreads) {
                out.printf("num_threads(%d)", ((OMPNumThreads) option).len());
            } else if(option instanceof OMPSchedule) {
                out.print("schedule(");
                OMPScheduleChoice choice = ((OMPSchedule) option).schedule();
                if(choice instanceof OMPStatic$) {
                    out.print("static");
                } else {
                    out.print("??");
                }
                out.print(")");
            } else {
                out.print("??");
            }
        }
    }

    @Override
    public void visit(OMPParallel parallel) {
        out.print("#pragma omp parallel");
        visitOmpOptions(parallel.optionsJava());
        out.newline();
        parallel.block().accept(this);
    }

    @Override
    public void visit(OMPSection section) {
        out.println("#pragma omp section");
        section.block().accept(this);
    }

    @Override
    public void visit(OMPSections sections) {
        out.println("#pragma omp sections");
        sections.block().accept(this);
    }

    @Override
    public void visit(OMPFor loop) {
        out.print("#pragma omp for");
        visitOmpOptions(loop.optionsJava());
        out.newline();
        loop.loop().accept(this);
    }

    @Override
    public void visit(OMPParallelFor loop) {
        out.print("#pragma omp parallel for");
        visitOmpOptions(loop.optionsJava());
        out.newline();
        loop.loop().accept(this);
    }

    @Override
    public void visit(OMPForSimd loop) {
        out.print("#pragma omp for simd");
        visitOmpOptions(loop.optionsJava());
        out.newline();
        loop.loop().accept(this);
    }

    @Override
    public void visit(InlineQuantifierPattern pattern) {
        out.print("{:");
        pattern.inner().apply(this);
        out.print(":}");
    }

    public void visit(Synchronized sync) {
        out.print("synchronized (");
        nextExpr();
        sync.expr().accept(this);
        out.lnprintf(")");
        sync.statement().accept(this);
    }

    @Override
    public void visit(CatchClause cc) {
        out.print("catch (");
        nextExpr();
        boolean first = true;
        for(Type t : cc.javaCatchTypes()) {
            if(!first) out.print(" | ");
            t.accept(this);
            first = false;
        }
        out.print(" ");
        out.print(cc.name());
        out.print(")");
        cc.block().accept(this);
    }

}
