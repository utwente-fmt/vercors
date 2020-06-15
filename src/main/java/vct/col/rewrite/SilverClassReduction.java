package vct.col.rewrite;

import hre.ast.MessageOrigin;

import java.io.File;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

import vct.main.Parsers;
import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.IntegerValue;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.stmt.decl.Method.Kind;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.util.*;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.*;
import hre.config.Configuration;

import vct.col.ast.util.ContractBuilder;
import vct.col.ast.util.UndefinedMapping;

/**
 * This rewriter converts a program with classes into
 * a program with records.
 * 
 * 
 * @author Stefan Blom
 *
 */
public class SilverClassReduction extends AbstractRewriter {

  private ASTMapping<ASTNode> seq=new UndefinedMapping<ASTNode>(){

    @Override
    public ASTNode post_map(ASTNode n, ASTNode res) {
      if (res==null) {
        Type t=n.getType();
        if (t.isPrimitive(PrimitiveSort.Sequence)){
          t=VectorExpression(rewrite((Type)((PrimitiveType)t).firstarg()));
          return create.invokation(t,null,"vseq", rewrite(n));
        }
        Fail("cannot map vector expression %s",n);
        return null;
      } else {
        return res;
      }
    }

    @Override
    public ASTNode map(OperatorExpression e) {
      switch(e.operator()){
      case VectorRepeat:{
        floats=true;
        Type t=VectorExpression(rewrite(e.arg(0).getType()));
        return create.invokation(t,null,"vrep",rewrite(e.arg(0)));
      }
      case VectorCompare:{
        floats=true;
        Type t=VectorExpression(rewrite((Type)((PrimitiveType)e.getType()).firstarg()));
        return create.invokation(t,null,"vcmp",e.arg(0).apply(this),e.arg(1).apply(this));
      }
      default:
        return null;
      }
    }
  };
  private ASTMapping<ASTNode> mat=new UndefinedMapping<ASTNode>(){

    @Override
    public ASTNode post_map(ASTNode n, ASTNode res) {
      if (res==null) {
        Type t=n.getType();
        if (t.isPrimitive(PrimitiveSort.Sequence)){
          t=(Type)((PrimitiveType)t).firstarg();
          t=MatrixExpression(rewrite((Type)((PrimitiveType)t).firstarg()));
          return create.invokation(t,null,"mseq", rewrite(n));
        }
        Fail("cannot map vector expression %s",n);
        return null;
      } else {
        return res;
      }
    }

    @Override
    public ASTNode map(OperatorExpression e) {
      switch(e.operator()){
      case MatrixRepeat:{
        floats=true;
        Type t=MatrixExpression(rewrite(e.arg(0).getType()));
        return create.invokation(t,null,"mrep",rewrite(e.arg(0)));
      }
      case MatrixCompare:{
        floats=true;
        Type t=(Type)((PrimitiveType)e.getType()).firstarg();
        t=MatrixExpression(rewrite((Type)((PrimitiveType)t).firstarg()));
        return create.invokation(t,null,"mcmp",e.arg(0).apply(this),e.arg(1).apply(this));
      }
      default:
        return null;
      }
    }
  };
  
  private AbstractRewriter index=new AbstractRewriter(source()){
    @Override
    public void visit(OperatorExpression e){
      switch(e.operator()){
      case RangeSeq:
        result=create.domain_call("VectorIndex","vrange",e.argsJava());
        break;
      default:
        result=e;
        break;
      }
    }
  };
  
  private AbstractRewriter mindex=new AbstractRewriter(source()){
    @Override
    public void visit(OperatorExpression e){
      switch(e.operator()){
      case Mult:
        result=create.domain_call("MatrixIndex","product",index.rewrite(e.argsJava()));
        break;
      default:
        result=e;
        break;
      }
    }
  };
  
  private static final String SEP="__";
      
  private static final String ILLEGAL_CAST="possibly_illegal_cast";
  
  private ASTClass ref_class;
  
  private ClassType ref_type;
  
  private HashSet<Type> ref_items=new HashSet<Type>();
  
  public SilverClassReduction(ProgramUnit source) {
    super(source);
    create.setOrigin(new MessageOrigin("collected class Ref"));
    ref_class=create.ast_class("Ref", ASTClass.ClassKind.Record,null, null, null);
    target().add(ref_class);
    ref_type=create.class_type("Ref");
  }

  @Override
  public void visit(AxiomaticDataType adt){
    super.visit(adt);
    if (adt.name().equals("TYPE")){
      AxiomaticDataType res=(AxiomaticDataType)result;
      res.add_cons(create.function_decl(
          create.class_type("TYPE"),
          null,
          "type_of",
          new DeclarationStatement[]{create.field_decl("val",create.class_type("Ref"))},
          null
      ));
      ref_class.add(create.field_decl(ILLEGAL_CAST,create.class_type("Ref")));
    }
  }
  
  @Override
  public void visit(NameExpression e){
    if (e.isReserved(ASTReserved.OptionNone)){
      Type t=rewrite(e.getType());
      result=create.invokation(t, null, "VCTNone");
    } else if (e.isReserved(ASTReserved.This)){
      if (constructor_this.peek()){
        if (in_requires){
          e.getOrigin().report("error","pre-condition of constructor may not refer to this");
          Fail("fatal error");
        }
        result=create.reserved_name(ASTReserved.Result);
      } else {
        result=create.unresolved_name(THIS);
      }
    } else if(e.isReserved(ASTReserved.FullPerm) || e.isReserved(ASTReserved.ReadPerm)) {
      fractions = true;
      result = create.invokation(null, null, "new_frac", e);
    } else if(e.isReserved(ASTReserved.NoPerm)) {
      fractions = true;
      result = create.invokation(null, null, "new_zfrac", e);
    } else {
      super.visit(e);
    }
  }

  private Stack<Boolean> constructor_this=new Stack<Boolean>();
  {
    constructor_this.push(false);
  }
  
  public static final String THIS="diz";
  
  private boolean options=false;
  private boolean arrays = false;
  private boolean floats = false;
  private boolean fractions = false;
  private boolean maps = false;
  private boolean tuple = false;


  private AtomicInteger option_count=new AtomicInteger();
  private HashMap<String, String> option_get = new HashMap<String, String>();
  private HashMap<String, Type> option_get_type = new HashMap<String, Type>();
  
  @Override
  public void visit(PrimitiveType t){
    switch(t.sort){
    case Cell:
      ref_items.add((Type)rewrite(t.firstarg()));
      result=ref_type;
      break;
    case Double:
    case Float:
      floats=true;
      result=create.class_type("VCTFloat");
      break;
    case Option:
    {
      options=true;
      List<ASTNode> args = rewrite(((PrimitiveType)t).argsJava());
      args.get(0).addLabel(create.label("T"));
      result=create.class_type("VCTOption",args);
      break;
    }
    case Array:{
      arrays = true;
      ASTNode ct = rewrite(t.firstarg());
      if(!ct.hasLabel("CT")) {
        ct.addLabel(create.label("CT"));
      }
      result = create.class_type("VCTArray", ct);
      break;
    }
    case Tuple: {
      tuple = true;
      List<ASTNode> args = rewrite(((PrimitiveType)t).argsJava());
      args.get(0).addLabel(create.label("F"));
      args.get(1).addLabel(create.label("S"));
      result=create.class_type("VCTTuple",args);
      break;
    }
    case Map:
      maps = true;
      tuple = true;
      List<ASTNode> args = rewrite(((PrimitiveType)t).argsJava());
      args.get(0).addLabel(create.label("K"));
      args.get(1).addLabel(create.label("V"));
      result=create.class_type("VCTMap",args);
      break;
    default:
      super.visit(t);
      break;
    }    
  }
  
  // multidim_index_2 is a generated function, of which one copy suffices.
  // TODO: fix this problem properly.
  private boolean multidim_index_2=false;
  
  @Override
  public void visit(ASTClass cl){
    for(ASTNode n:cl.staticMembers()){
      if (n instanceof Method){
        Method m=(Method)n;
        if (m.name().equals("multidim_index_2")){
          if (multidim_index_2) continue;
          multidim_index_2=true;
        }
        target().add(rewrite(n));
      } else if (n instanceof DeclarationStatement) {
        Fail("Illegal static field %s",n);
      } else if(n.isSpecial(ASTSpecial.Kind.Comment)) {
        target().add(rewrite(n));
      } else {
        Fail("Illegal static member %s",n);
      }
    }
    for(ASTNode n:cl.dynamicMembers()){
      if (n instanceof DeclarationStatement){
        ref_class.add(rewrite(n));
      } else if (n instanceof Method){
        Method m=(Method)n;
        if (m.name().equals("multidim_index_2")){
          if (multidim_index_2) continue;
          multidim_index_2=true;
        }
        ASTNode res=rewrite(n);
        res.setStatic(true);
        target().add(res);
      } else if(n.isSpecial(ASTSpecial.Kind.Comment)) {
        target().add(rewrite(n));
      } else {
        Fail("Illegal dynamic member %s",n);
      }
    }
  }
  
  @Override
  public void visit(ClassType t){
    if (source().find(t.getNameFull())==null){
      // ADT type
      super.visit(t);
    } else {
      result=create.class_type("Ref");
    }
  }

  @Override
  public void visit(Dereference e){
    if (e.obj().getType()==null){
      Fail("untyped object %s at %s", e.obj(), e.obj().getOrigin());
      result=create.dereference(rewrite(e.obj()), e.field());
      return;
    }
    Type t=e.obj().getType();
    if (t.isPrimitive(PrimitiveSort.Cell)){
      PrimitiveType tt=(PrimitiveType)t;
      Type type=(Type)rewrite(tt.firstarg());
      String name=silverTypeString(type);
      ref_items.add(type);
      result=create.dereference(rewrite(e.obj()), name+SEP+e.field());
    } else {
      result=create.dereference(rewrite(e.obj()), e.field());
    }
  }
  
  private Type VectorExpression(Type t){
    ASTNode args[]=new ASTNode[]{t};
    args[0].addLabel(create.label("T"));
    t=create.class_type("VectorExpression",args);
    return t;
  }
  
  private Type MatrixExpression(Type t){
    ASTNode args[]=new ASTNode[]{t};
    args[0].addLabel(create.label("T"));
    t=create.class_type("MatrixExpression",args);
    return t;
  }

  private ASTNode getFracVal(ASTNode node) {
    fractions = true;
    String method = node.getType().isPrimitive(PrimitiveSort.ZFraction) ? "zfrac_val" : "frac_val";
    node = rewrite(node);

    if(node instanceof MethodInvokation && (((MethodInvokation) node).method.equals("new_frac") || ((MethodInvokation) node).method.equals("new_zfrac"))) {
      return ((MethodInvokation) node).getArg(0);
    } else {
      return create.invokation(null, null, method, node);
    }
  }

  private ASTNode packFracVal(Type t, ASTNode node) {
    fractions = true;
    String method = t.isPrimitive(PrimitiveSort.ZFraction) ? "new_zfrac" : "new_frac";
    return create.invokation(null, null, method, node);
  }
  
  @Override
  public void visit(OperatorExpression e){
    switch(e.operator()){
    case VectorRepeat:{
      floats=true;
      Type t=VectorExpression(rewrite(e.getType()));
      result=create.invokation(t,null,"vrep",rewrite(e.argsJava()));
      break;
    }
    case VectorCompare:{
      floats=true;
      Type t=VectorExpression(rewrite((Type)((PrimitiveType)e.getType()).firstarg()));
      result=create.invokation(t,null,"vcmp",rewrite(e.argsJava()));
      break;
    }
    case MatrixRepeat:{
      floats=true;
      Type t=MatrixExpression(rewrite(e.getType()));
      result=create.invokation(t,null,"mrep",rewrite(e.argsJava()));
      break;
    }
    case MatrixCompare:{
      floats=true;
      Type t=(Type)((PrimitiveType)e.getType()).firstarg();
      t=VectorExpression(rewrite((Type)((PrimitiveType)t).firstarg()));
      result=create.invokation(t,null,"mcmp",rewrite(e.argsJava()));
      break;
    }
    case MatrixSum:{
      floats=true;
      if (e.getType().isNumeric()){
        Type t=rewrite(e.getType());
        t=MatrixExpression(t);
        result=create.invokation(t,null,"msum",
            mindex.rewrite(rewrite(e.arg(0))),e.arg(1).apply(mat));
      } else {
        Fail("cannot do a summation of type %s",e.getType()); 
      }
      break;      
     
    }
    case FoldPlus:{
      floats=true;
      if (e.getType().isNumeric()){
        Type t=rewrite(e.getType());
        ASTNode args[]=new ASTNode[]{t};
        args[0].addLabel(create.label("T"));
        t=create.class_type("VectorExpression",args);
        
        result=create.invokation(t,null,"vsum",
            index.rewrite(rewrite(e.arg(0))),e.arg(1).apply(seq));
      } else {
        Fail("cannot do a summation of type %s",e.getType()); 
      }
      break;      
    }
    case OptionSome:{
      options=true;
      Type t=rewrite(e.getType());
      result=create.invokation(t, null,"VCTSome",rewrite(e.argsJava()));
      break;
    }
    case OptionGet:{
      options=true;
      Type t=rewrite(e.arg(0).getType());
      String method=optionGet(t);
      result=create.invokation(null, null,method,rewrite(e.argsJava()));
      break;
    }
    case New:{
      ClassType t=(ClassType)e.arg(0);
      ASTClass cl=source().find(t);
      ArrayList<ASTNode>args=new ArrayList<ASTNode>();
      //NameExpression f=create.field_name("A__x");
      //f.setSite(ref_class);
      for(DeclarationStatement field:cl.dynamicFields()){
        args.add(create.dereference(create.class_type("Ref"),field.name()));
      }
      result=create.expression(StandardOperator.NewSilver,args.toArray(new ASTNode[0]));
      break;
    }
    case TypeOf:{
      result=create.domain_call("TYPE","type_of",rewrite(e.arg(0)));
      break;
    }
    case Cast:{
      Type t0=e.arg(1).getType();
      ASTNode object=rewrite(e.arg(1));
      Type t=(Type)e.arg(0);
      if (t.isPrimitive(PrimitiveSort.Float)) {
        if (t0.isPrimitive(PrimitiveSort.Integer)) {
          result = create.domain_call("VCTFloat", "ffromint", object);
        } else {
          Fail("cannot convert %s to float yet.", t0);
        }
      } else if(t.isPrimitive(PrimitiveSort.Option)) {
        // Type marker, ignore.
        result = rewrite(e.arg(1));
      } else {
        ASTNode condition=create.invokation(null, null,"instanceof",
            create.domain_call("TYPE","type_of",object),
            //create.invokation(null,null,"type_of",object));
            create.domain_call("TYPE","class_"+t));
            
        ASTNode illegal=create.dereference(object,ILLEGAL_CAST);
        result=create.expression(StandardOperator.ITE,condition,object,illegal);
      }
      break;
    }
    case Subscript:{
      if(e.first().getType().isPrimitive(PrimitiveSort.Array)) {
        arrays = true;
        ASTNode type = rewrite(e.first().getType());
        List<ASTNode> args = rewrite(e.argsJava());
        result = create.invokation(type, null, "loc", args);
      } else {
        super.visit(e);
      }
      break;
    }
    case Length:{
      if(e.first().getType().isPrimitive(PrimitiveSort.Array)) {
        arrays = true;
        ASTNode type = rewrite(e.first().getType());
        List<ASTNode> args = rewrite(e.argsJava());
        result = create.invokation(type, null, "alen", args);
      } else {
        super.visit(e);
      }
      break;
    }
    case Div:
    case Plus:
    case Minus:
    case Mult:
    case LT:
    case LTE:
    case EQ:
    case NEQ:
    case GT:
    case GTE: {
      if(e.operator() == StandardOperator.Plus && e.getType() != null && e.getType().isPrimitive(PrimitiveSort.Float)){
        result = create.domain_call("VCTFloat", "fadd", rewrite(e.argsJava()));
        return;
      }

      ASTNode left = e.arg(0), right = e.arg(1);

      if(left.getType() != null && left.getType().isFraction()) {
        left = getFracVal(left);
      } else {
        left = rewrite(left);
      }

      if(right.getType() != null && right.getType().isFraction()) {
        right = getFracVal(right);
      } else {
        right = rewrite(right);
      }

      if(e.getType() != null && e.getType().isFraction()) {
        result = packFracVal(e.getType(), create.expression(e.operator(), left, right));
      } else {
        result = create.expression(e.operator(), left, right);
      }
      break;
    }
    case HistoryPerm:
    case ActionPerm:
    case Perm: {
      ASTNode permVal = e.arg(1);
      if(permVal.getType().isFraction()) {
        permVal = getFracVal(permVal);
      } else {
        permVal = rewrite(permVal);
      }

      result = create.expression(e.operator(), rewrite(e.arg(0)), permVal);
      break;
    }
    case Scale: {
      ASTNode permVal = e.arg(0);
      if (permVal.getType().isFraction()) {
        permVal = getFracVal(permVal);
      } else {
        permVal = rewrite(permVal);
      }
      result = create.expression(StandardOperator.Scale, permVal, rewrite(e.arg(1)));
      break;
    }
    case PointsTo: {
      ASTNode permVal = e.arg(1);
      if(permVal.getType().isFraction()) {
        permVal = getFracVal(permVal);
      } else {
        permVal = rewrite(permVal);
      }

      result = create.expression(e.operator(), rewrite(e.arg(0)), permVal, rewrite(e.arg(2)));
      break;
    }
    case MapBuild:{
      List<ASTNode> args = rewrite(e.argsJava());
      result = create.invokation(rewrite(e.getType()), null, "vctmap_build", args);
      break;
    }
    case MapEquality: {
      List<ASTNode> args = rewrite(e.argsJava());
      result = create.invokation(rewrite(e.first().getType()), null, "vctmap_equals", args);
      break;
    }
    case MapDisjoint:{
      List<ASTNode> args = rewrite(e.argsJava());
      result = create.invokation(rewrite(e.first().getType()), null, "vctmap_disjoint", args);
      break;
    }
    case MapKeySet:{
      List<ASTNode> args = rewrite(e.argsJava());
      result = create.invokation(rewrite(e.first().getType()), null, "vctmap_keys", args);
      break;
    }
    case MapCardinality:{
      List<ASTNode> args = rewrite(e.argsJava());
      result = create.invokation(rewrite(e.first().getType()), null, "vctmap_card", args);
      break;
    }
    case MapValueSet:{
      List<ASTNode> args = rewrite(e.argsJava());
      result = create.invokation(rewrite(e.first().getType()), null, "vctmap_values", args);
      break;
    }
    case MapGetByKey:{
      List<ASTNode> args = rewrite(e.argsJava());
      result = create.invokation(rewrite(e.first().getType()), null, "vctmap_get", args);
      break;
    }
    case MapRemoveKey:{
      List<ASTNode> args = rewrite(e.argsJava());
      result = create.invokation(rewrite(e.first().getType()), null, "vctmap_remove", args);
      break;
    }
      case MapItemSet:{
      List<ASTNode> args = rewrite(e.argsJava());
      result = create.invokation(rewrite(e.first().getType()), null, "vctmap_items", args);
      break;
    }
      case TupleFst:{
        ASTNode type = rewrite(e.first().getType().firstarg());
        List<ASTNode> args = rewrite(e.argsJava());
        result = create.invokation(rewrite(e.first().getType()), null, "vcttuple_fst", args);
        break;
    }
    case TupleSnd:{
        ASTNode type = rewrite(e.first().getType().secondarg());
        List<ASTNode> args = rewrite(e.argsJava());
        result = create.invokation(rewrite(e.first().getType()), null, "vcttuple_snd", args);
        break;
      }
    default:
      super.visit(e);
    }
  }
  
  private String optionGet(Type type) {
    // Somehow different Scala types are not distinguished in HashMaps.
    // Therefore they're distinguished by name.
    String t = type.toString();
    String method=option_get.get(t);
    if (method==null){
      method="getVCTOption"+option_count.incrementAndGet();
      option_get.put(t, method);
      option_get_type.put(t,  type);
    }
    return method;
  }

  @Override
  public void visit(ConstantExpression val) {
    if(val.value() instanceof IntegerValue && val.getType().isFraction()) {
      val.setType(new PrimitiveType(PrimitiveSort.Rational));
      result = packFracVal(val.getType(), val);
    } else {
      super.visit(val);
    }
  }

  @Override
  public void visit(StructValue v) {
    if (v.type().isPrimitive(PrimitiveSort.Map)) {
      Type resultType = rewrite(v.type());
      ASTNode map = create.invokation(resultType,null,"vctmap_empty");
      for (int i=0; i < v.valuesArray().length; i+=2) {
        map = create.invokation(resultType, null, "vctmap_build", map, v.valuesArray()[i], v.valuesArray()[i+1]);
      }
      result = map;
    } else if (v.type().isPrimitive(PrimitiveSort.Tuple)) {
      Type resultType = rewrite(v.type());
      result =  create.invokation(resultType,null,"vcttuple_tuple", rewrite(v.valuesArray()));;
    } else {
      super.visit(v);
    }
  }

  @Override
  public void visit(Method m){
    String name=m.getName();
    ContractBuilder cb=new ContractBuilder();
    ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
    ASTNode body=m.getBody();
    if (!m.isStatic() && m.kind!=Kind.Constructor){
      args.add(create.field_decl(THIS, ref_type));
      ASTNode nonnull=create.expression(StandardOperator.NEQ,
        create.local_name(THIS),
        create.reserved_name(ASTReserved.Null));
      if (m.kind!=Method.Kind.Predicate){
         cb.requires(nonnull);
      } else {
        if (body != null) {
          body=create.expression(StandardOperator.Star,nonnull,body);
        }
      }
    }
    if (m.kind==Kind.Constructor){
      cb.ensures(create.expression(StandardOperator.NEQ,
          create.reserved_name(ASTReserved.Result),
          create.reserved_name(ASTReserved.Null)));
    }
    for(DeclarationStatement d:m.getArgs()){
      args.add(rewrite(d));
    }
    Contract c=m.getContract();
    if (c!=null){
      constructor_this.push(m.kind==Kind.Constructor);
      cb.given(rewrite(c.given));
      cb.yields(rewrite(c.yields));
      if (c.modifies!=null) cb.modifies(rewrite(c.modifies)); 
      if (c.accesses!=null) cb.accesses(rewrite(c.accesses)); 
      in_requires=true;
      for(ASTNode clause:ASTUtils.conjuncts(c.invariant,StandardOperator.Star)){
        cb.requires(rewrite(clause));
      }
      for(ASTNode clause:ASTUtils.conjuncts(c.pre_condition,StandardOperator.Star)){
        cb.requires(rewrite(clause));
      }
      in_requires=false;
      in_ensures=true;
      for(ASTNode clause:ASTUtils.conjuncts(c.invariant,StandardOperator.Star)){
        cb.ensures(rewrite(clause));
      }
      for(ASTNode clause:ASTUtils.conjuncts(c.post_condition,StandardOperator.Star)){
        cb.ensures(rewrite(clause));
      }
      in_ensures=false;
      if (c.signals!=null) for(DeclarationStatement decl:c.signals){
        cb.signals((ClassType)rewrite(decl.getType()), decl.name(), rewrite(decl.initJava()));      
      }
      constructor_this.pop();
    }
    Method.Kind kind;
    Type rt;
    if(m.kind==Kind.Constructor){
      kind=Kind.Plain;
      rt=ref_type;
    } else {
      kind=m.kind;
      rt=rewrite(m.getReturnType());
    }
    currentContractBuilder=null;
    body=rewrite(body);
    if (m.kind==Method.Kind.Constructor){
      ASTClass cl=(ASTClass)m.getParent();
      if (body!=null){
        body=create.block(
          create.field_decl(THIS,ref_type),
          create.assignment(
              create.local_name(THIS),
              rewrite(create.new_record(new ClassType(cl.getFullName())))
          ),
          body,
          create.return_statement(create.local_name(THIS))
        );
      }   
    }
    c=cb.getContract();
    result=create.method_kind(kind, rt, c, name, args, m.usesVarArgs(), body);

  }
  
  @Override
  public ProgramUnit rewriteAll(){
    ProgramUnit res=super.rewriteAll();
    for(Type t:ref_items){
      String s=silverTypeString(t);
      ref_class.add_dynamic(create.field_decl(s+SEP+"item",t));
    }
    if (options || floats || arrays || fractions || maps || tuple){
      String preludeFile = source().hasLanguageFlag(ProgramUnit.LanguageFlag.SeparateArrayLocations) ? "prelude.sil" : "prelude_C.sil";
      File file = Configuration.getConfigFile(preludeFile);
      ProgramUnit prelude=Parsers.getParser("sil").parse(file);
      for(ASTNode n:prelude){
        if (n instanceof AxiomaticDataType){
          AxiomaticDataType adt=(AxiomaticDataType)n;
          switch (adt.name()) {
          case "VCTOption":
            if (options) res.add(n);
            break;
          case "MatrixIndex":
          case "MatrixExpression":
          case "VectorIndex":
          case "VectorExpression":
          case "VCTFloat":
            if (floats) res.add(n);
            break;
          case "VCTArray":
            if (arrays) res.add(n);
            break;
          case "frac":
          case "zfrac":
            if(fractions) res.add(n);
            break;
          case "VCTMap":
            if(maps) res.add(n);
            break;
          case "VCTTuple":
            if (maps || tuple) res.add(n);
              break;
          }
        } else if(n instanceof Method) {
          Method method = (Method) n;
          switch(method.name()) {
            case "new_frac":
            case "new_zfrac":
              if(fractions) res.add(method);
              break;
          }
        }
      }
      for(Entry<String,String> entry:option_get.entrySet()){
        create.enter();
        Type t=rewrite(option_get_type.get(entry.getKey()));
        String extraMessage;

        if(t.firstarg() instanceof ClassType && ((ClassType) t.firstarg()).getFullName().equals("VCTArray")) {
          extraMessage = "Array access to value which may be null";
        } else {
          extraMessage = "Value may be None";
        }

        create.setOrigin(new MessageOrigin("Generated OptionGet code: " + extraMessage));
        Type returns=(Type)((ClassType)t).firstarg();
        String name=entry.getValue();
        ContractBuilder cb=new ContractBuilder();
        cb.requires(neq(create.local_name("x"),create.invokation(t,null,"VCTNone")));
        DeclarationStatement args[]=new DeclarationStatement[]{
          create.field_decl("x",t)
        };
        ASTNode body=create.invokation(t,null,"getVCTOption",create.local_name("x"));
        Contract contract=cb.getContract();
        Method method=create.function_decl(returns, contract, name, args, body);
        method.setStatic(true);
        res.add(method);
        create.leave();
      }
    }
    HashSet<String> names=new HashSet<String>();
    for(ASTNode n:res){
      if (n instanceof ASTDeclaration && !(n instanceof ASTSpecial)){
        ASTDeclaration d=(ASTDeclaration)n;
        if (names.contains(d.name())){
          Warning("name %s declared more than once",d.name());
        }
        names.add(d.name());
      }
    }
    return res;
  }
  
  @Override
  public void visit(MethodInvokation s){
    String method;
    ArrayList<ASTNode> args=new ArrayList<ASTNode>();
    Method def=s.getDefinition();
    ClassType dispatch=s.dispatch;
    if (def.kind==Kind.Constructor){
      dispatch=null;
    }
    ASTNode object=null;
    if (def.getParent()==null){
      method=s.method;
    } else if (s.object instanceof ClassType){
      if (s.method.equals(Method.JavaConstructor)){
        method=s.dispatch.getName()+SEP+s.dispatch.getName();
        dispatch=null;
      } else if (def.getParent() instanceof AxiomaticDataType){
        method=s.method;
        object=copy_rw.rewrite(s.object);
      } else {
        method=s.method;
      }
    } else if (s.object==null){
      if (s.method.equals(Method.JavaConstructor)){
        method=s.dispatch.getName()+SEP+s.dispatch.getName();
        dispatch=null;
      } else {
        method=s.method;
      }
    } else {
      method=s.method;
      if (method.equals("<<adt>>") || def.getParent() instanceof AxiomaticDataType){
        method=s.method;
      } else {
        if (!def.isStatic()){
          args.add(rewrite(s.object));
        }
        if (def.kind==Kind.Predicate && !s.object.isReserved(ASTReserved.This) && (!fold_unfold) ){
          //extra=create.expression(StandardOperator.NEQ,rewrite(s.object),create.reserved_name(ASTReserved.Null));
        }
      }      
    }
    for(ASTNode arg :s.getArgs()){
      args.add(rewrite(arg));
    }
    MethodInvokation res=create.invokation(object, dispatch, method, args.toArray(new ASTNode[0]));
    res.set_before(rewrite(s.get_before()));
    res.set_after(rewrite(s.get_after()));
    result=res;
  }
  
  private String silverTypeString(Type type) {
    String res = "";
    List<ASTNode> args = type.argsJava();
    for(ASTNode n: args) {
      res += "_"+n.toString();
    }
    res = type.toString().replaceAll("[<>, ]", "_");
    return res;
  }

}
