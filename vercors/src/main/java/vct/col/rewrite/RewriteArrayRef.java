package vct.col.rewrite;

import java.util.ArrayList;
import java.util.HashSet;

import hre.ast.MessageOrigin;
import vct.col.ast.ASTNode;
import vct.col.ast.ASTReserved;
import vct.col.ast.AssignmentStatement;
import vct.col.ast.ConstantExpression;
import vct.col.ast.ContractBuilder;
import vct.col.ast.DeclarationStatement;
import vct.col.ast.Dereference;
import vct.col.ast.Method;
import vct.col.ast.NameExpression;
import vct.col.ast.OperatorExpression;
import vct.col.ast.PrimitiveSort;
import vct.col.ast.PrimitiveType;
import vct.col.ast.ProgramUnit;
import vct.col.ast.StandardOperator;
import vct.col.ast.Type;

public class RewriteArrayRef extends AbstractRewriter {
  
  private HashSet<Type> new_array;
  private HashSet<Type> array_values;
  
  private final static String ARRAY_CONSTRUCTOR = "new_array_";
  private final static String ARRAY_VALUES = "array_values_";
  
  public RewriteArrayRef(ProgramUnit source) {
    super(source);
  }
  
  @Override
  public ProgramUnit rewriteAll() {
    new_array = new HashSet<Type>();
    array_values = new HashSet<Type>();
    ProgramUnit res = super.rewriteAll();
    create.enter();
    create.setOrigin(new MessageOrigin("Array constructors"));
    for (Type t : new_array) {
      res.add(arrayConstructorFor(t));
    }
    create.leave();
    create.enter();
    create.setOrigin(new MessageOrigin("Array values"));
    for (Type t : array_values) {
      res.add(arrayValuesFor(t));
    }
    create.leave();
    new_array = null;
    array_values = null;
    return res;
  }
  
  @Override
  public void visit(OperatorExpression e) {
    switch (e.operator()) {
    case ValidArray: {
      ASTNode M = rewrite(e.arg(0));
      ASTNode sz = rewrite(e.arg(1));
      // Determine type in order to generate appropiate conditions for ValidArray.
      Type t = e.first().getType();
      t = rewrite(t);
      M.setType(t);
      result = validArrayFor(M, sz);
      break;
    }
    case ValidMatrix: {
      ASTNode M = rewrite(e.arg(0));
      ASTNode sz1 = rewrite(e.arg(1));
      ASTNode sz2 = rewrite(e.arg(2));
      // Determine type in order to generate appropiate conditions for ValidMatrix.
      Type t = e.first().getType();
      t = rewrite(t);
      M.setType(t);
      result = validMatrixFor(M, sz1, sz2);
      break;
    }
    case EQ:
    case NEQ: {
      ASTNode e0 = e.arg(0);
      ASTNode e1 = e.arg(1);
      ASTNode array = null;
      if (e0.isReserved(ASTReserved.Null) && e1.getType().isPrimitive(PrimitiveSort.Array)) {
        array = e1;
      }
      if (e1.isReserved(ASTReserved.Null) && e0.getType().isPrimitive(PrimitiveSort.Array)) {
        array = e0;
      }
      if (array != null && rewrite(array.getType()).isPrimitive(PrimitiveSort.Option)) {
        result = create.expression(e.operator(), create.reserved_name(ASTReserved.OptionNone), rewrite(array));
      } else {
        super.visit(e);
      }
      break;
    }
    case Length:
      if (e.first().getType().isPrimitive(PrimitiveSort.Array)) {
        ASTNode res = create.expression(StandardOperator.OptionGet, rewrite(e.first()));
        result = create.expression(StandardOperator.Length, res);
        break;
      } else {
        super.visit(e);
      }
    case Subscript:
      if (e.first().getType().isPrimitive(PrimitiveSort.Array)) {
        // Determine the (new) type to determine how to address elements in the array.
        Type t = rewrite(e.first().getType());
        ASTNode base = rewrite(e.first());
        if (t.isPrimitive(PrimitiveSort.Option)) {
          base = create.expression(StandardOperator.OptionGet, base);
          t = (Type) t.firstarg();
        }
        ASTNode index = rewrite(e.second());
        result = create.expression(StandardOperator.Subscript, base, index);
        t = (Type) t.firstarg();
        if (t.isPrimitive(PrimitiveSort.Cell)) {
          result = create.dereference(result, "item");
        }
      } else {
        super.visit(e);
      }
      break;
    case Values: {
      Type t = (Type) ((PrimitiveType) e.getType()).firstarg();
      array_values.add(t);
      result = create.invokation(null, null, ARRAY_VALUES + t, rewrite(e.argsJava()));
      break;
    }
    case NewArray: {
      Type t = (Type) e.arg(0);
      new_array.add(t);
      result = create.invokation(null, null, ARRAY_CONSTRUCTOR + t, rewrite(e.arg(1)));
      break;
    }
    case PointsTo: {
      // If an Array field should have a Null value, change it to OptionNone.
      if (e.first().getType().isPrimitive(PrimitiveSort.Array) && e.third().isReserved(ASTReserved.Null)
          && rewrite(e.first().getType()).isPrimitive(PrimitiveSort.Option)) {
        ASTNode res = rewrite(e.first());
        result = star(create.expression(StandardOperator.Perm, res, rewrite(e.second())),
            create.expression(StandardOperator.EQ, res, create.reserved_name(ASTReserved.OptionNone)));
      } else {
        super.visit(e);
      }
      break;
    }
    default:
      super.visit(e);
    }
  }
  
  @Override
  public void visit(Dereference e) {
    if (e.field().equals("length")) {
      ASTNode res = rewrite(e.obj());
      if (rewrite(e.obj().getType()).isPrimitive(PrimitiveSort.Option)) {
        res = create.expression(StandardOperator.OptionGet, res);
      }
      result = create.expression(StandardOperator.Length, res);
    } else {
      super.visit(e);
    }
  }
  
  @Override
  public void visit(PrimitiveType t) {
    switch (t.sort) {
    case Array: {
      // For now use the type that is most backward compatible:
      // * Arrays are nullable.
      // * Only the deepest level elements are actual heap references (Cell-type).
      // This might change in the future when types are fixed in the front-end.
      if (((Type) t.firstarg()).isPrimitive(PrimitiveSort.Array)) {
        result = create.primitive_type(PrimitiveSort.Option,
            create.primitive_type(PrimitiveSort.Array, rewrite(t.firstarg())));
      } else {
        result = create.primitive_type(PrimitiveSort.Option, create.primitive_type(PrimitiveSort.Array,
            create.primitive_type(PrimitiveSort.Cell, rewrite(t.firstarg()))));
      }
      break;
    }
    default:
      super.visit(t);
      break;
    }
  }
  
  @Override
  public void visit(AssignmentStatement as) {
    // Assign OptionNone if array is an option and is assigned null.
    if (as.location().getType().isPrimitive(PrimitiveSort.Array) && as.expression().isReserved(ASTReserved.Null)
        && rewrite(as.location().getType()).isPrimitive(PrimitiveSort.Option)) {
      result = create.assignment(rewrite(as.location()), create.reserved_name(ASTReserved.OptionNone));
    } else {
      super.visit(as);
    }
  }
  
  private Method arrayConstructorFor(Type t) {
    if (t.isPrimitive(PrimitiveSort.Array)) {
      // TODO: initialization of multidim arrays.
      return null;
    } else {
      Type nt = create.primitive_type(PrimitiveSort.Array, t);
      nt = rewrite(nt);
      Type result_type = nt;
      ASTNode array = create.reserved_name(ASTReserved.Result);
      ContractBuilder cb = new ContractBuilder();
      DeclarationStatement args[] = new DeclarationStatement[] {
          create.field_decl("len", create.primitive_type(PrimitiveSort.Integer)) };
      NameExpression len = create.local_name("len");
      NameExpression i = create.local_name("i");
      ConstantExpression nul = create.constant(0);
      if (nt.isPrimitive(PrimitiveSort.Option)) {
        cb.ensures(neq(array, create.reserved_name(ASTReserved.OptionNone)));
        array = create.expression(StandardOperator.OptionGet, array);
      }
      ASTNode length = create.expression(StandardOperator.Length, array);
      ASTNode base = create.dereference(create.expression(StandardOperator.Subscript, array, i), "item");
      ASTNode guard = and(lte(nul, i), less(i, length));
      ASTNode claim = create.expression(StandardOperator.Perm, base, create.reserved_name(ASTReserved.FullPerm));
      DeclarationStatement decl = create.field_decl("i", create.primitive_type(PrimitiveSort.Integer));
      cb.ensures(eq(length, len));
      cb.ensures(create.starall(guard, claim, decl));
      Method m = create.method_decl(result_type, cb.getContract(), ARRAY_CONSTRUCTOR + t, args, null);
      m.setStatic(true);
      return m;
    }
  }
  
  private Method arrayValuesFor(Type t) {
    PrimitiveType result_type = create.primitive_type(PrimitiveSort.Sequence, rewrite(t));
    Type type0 = create.primitive_type(PrimitiveSort.Array, t);
    type0 = rewrite(type0);
    ContractBuilder cb = new ContractBuilder();
    ArrayList<DeclarationStatement> args = new ArrayList<DeclarationStatement>();
    args.add(create.field_decl("ar", type0));
    args.add(create.field_decl("from", create.primitive_type(PrimitiveSort.Integer)));
    args.add(create.field_decl("upto", create.primitive_type(PrimitiveSort.Integer)));
    
    ASTNode deref = create.local_name("ar");
    if (type0.isPrimitive(PrimitiveSort.Option)) {
      cb.requires(neq(create.local_name("ar"), create.reserved_name(ASTReserved.OptionNone)));
      deref = create.expression(StandardOperator.OptionGet, deref);
    }
    
    cb.requires(create.expression(StandardOperator.LTE, create.constant(0), create.local_name("from")));
    cb.requires(create.expression(StandardOperator.LTE, create.local_name("from"), create.local_name("upto")));
    cb.requires(create.expression(StandardOperator.LTE, create.local_name("upto"),
        create.expression(StandardOperator.Length, deref)));
    ASTNode range = create.expression(StandardOperator.And,
        create.expression(StandardOperator.LTE, create.local_name("from"), create.local_name("i")),
        create.expression(StandardOperator.LT, create.local_name("i"), create.local_name("upto")));
    DeclarationStatement decl = create.field_decl("i", create.primitive_type(PrimitiveSort.Integer));
    ASTNode ari = create.dereference(create.expression(StandardOperator.Subscript, deref, create.local_name("i")),
        "item");
    ASTNode perm = create.expression(StandardOperator.Perm, ari, create.reserved_name(ASTReserved.ReadPerm));
    cb.requires(create.starall(range, perm, decl));
    ASTNode Resi = create.expression(StandardOperator.Subscript, create.reserved_name(ASTReserved.Result),
        create.expression(StandardOperator.Minus, create.local_name("i"), create.local_name("from")));
    cb.ensures(create.expression(StandardOperator.EQ,
        create.expression(StandardOperator.Size, create.reserved_name(ASTReserved.Result)),
        create.expression(StandardOperator.Minus, create.local_name("upto"), create.local_name("from"))));
    cb.ensures(create.forall(range, create.expression(StandardOperator.EQ, ari, Resi), decl));
    
    ASTNode len = create.expression(StandardOperator.Minus, create.local_name("upto"), create.local_name("from"));
    range = create.expression(StandardOperator.And,
        create.expression(StandardOperator.LTE, create.constant(0), create.local_name("i")),
        create.expression(StandardOperator.LT, create.local_name("i"), len));
    ari = create.dereference(create.expression(StandardOperator.Subscript, deref,
        create.expression(StandardOperator.Plus, create.local_name("i"), create.local_name("from"))), "item");
    Resi = create.expression(StandardOperator.Subscript, create.reserved_name(ASTReserved.Result),
        create.local_name("i"));
    cb.ensures(create.forall(range, create.expression(StandardOperator.EQ, ari, Resi), decl));
    
    Method m = create.function_decl(result_type, cb.getContract(), ARRAY_VALUES + t, args, null);
    m.setStatic(true);
    return m;
  }
  
  /**
   * Generate conditions for a "valid array" based on its type.
   * 
   * @param array
   *          The array to generate conditions for (needs to have a type set).
   * @param size
   *          The size that the given array is supposed to have.
   * @return Conditions that define a "valid array" for the given node (with its
   *         type). Since the VCTArray domain in Viper already defines
   *         injectivity, it is only needed to optionally define that it is not
   *         equal to None, and that the array has the given size.
   */
  private ASTNode validArrayFor(ASTNode array, ASTNode size) {
    Type type = array.getType();
    ASTNode base = array;
    ASTNode res = create.constant(true);
    if (type.isPrimitive(PrimitiveSort.Option)) {
      res = neq(array, create.reserved_name(ASTReserved.OptionNone));
      base = create.expression(StandardOperator.OptionGet, base);
      type = (Type) type.firstarg();
    }
    return and(res, eq(create.expression(StandardOperator.Length, base), size));
  }
  
  /**
   * Generate conditions for a "valid matrix" based on its type. Note that a valid
   * matrix must be injective for all its cells. Thus if rows are heap locations
   * (Cell-type) we must also show injectivity for the cells within the rows.
   * 
   * @param matrix
   *          The object to generate the conditions for.
   * @param sz1,sz2
   *          The dimensions that the matrix is supposed to have.
   * @return Conditions to define a "valid matrix" with the given demensions.
   */
  private ASTNode validMatrixFor(ASTNode matrix, ASTNode sz1, ASTNode sz2) {
    Type type = matrix.getType();
    ASTNode res = create.constant(true);
    ASTNode base = matrix;
    if (type.isPrimitive(PrimitiveSort.Option)) {
      res = neq(matrix, create.reserved_name(ASTReserved.OptionNone));
      base = create.expression(StandardOperator.OptionGet, base);
      type = (Type) type.firstarg();
    }
    if (!type.isPrimitive(PrimitiveSort.Array)) {
      Abort("Unexpected type for ValidMatrix");
    }
    type = (Type) type.firstarg();
    ASTNode nul = create.constant(0);
    ASTNode i = create.local_name("vm_i");
    ASTNode k = create.local_name("vm_k");
    ASTNode rowi = create.expression(StandardOperator.Subscript, base, i);
    ASTNode rowk = create.expression(StandardOperator.Subscript, base, k);
    DeclarationStatement decli = create.field_decl("vm_i", create.primitive_type(PrimitiveSort.Integer));
    ASTNode guardi = and(lte(nul, i), less(i, sz1));
    ASTNode mInjectivity = null;
    if (type.isPrimitive(PrimitiveSort.Cell)) {
      type = (Type) type.firstarg();
      // add read permissions for every row
      rowi = create.dereference(rowi, "item");
      rowk = create.dereference(rowk, "item");
      ASTNode claim = create.expression(StandardOperator.Perm, rowi, create.reserved_name(ASTReserved.ReadPerm));
      res = star(res, create.starall(guardi, claim, decli));
      // Show injectivity for heap locations.
      ASTNode j = create.local_name("vm_j");
      ASTNode l = create.local_name("vm_l");
      ArrayList<ASTNode> cons = new ArrayList<ASTNode>();
      cons.add(guardi);
      cons.add(lte(nul, j));
      cons.add(less(j, sz2));
      cons.add(lte(nul, k));
      cons.add(less(k, sz1));
      cons.add(lte(nul, l));
      cons.add(less(l, sz2));
      ASTNode ij = rowi;
      ASTNode kl = rowk;
      if (type.isPrimitive(PrimitiveSort.Option)) {
        ij = create.expression(StandardOperator.OptionGet, ij);
        kl = create.expression(StandardOperator.OptionGet, kl);
      }
      ij = create.expression(StandardOperator.Subscript, ij, j);
      kl = create.expression(StandardOperator.Subscript, kl, l);
      cons.add(eq(ij, kl));
      ASTNode guard = create.fold(StandardOperator.And, cons);
      claim = and(eq(i, k), eq(j, l));
      ASTNode[] trigger = new ASTNode[] { ij, kl };
      ASTNode triggers[][] = new ASTNode[][] { trigger };
      mInjectivity = create.forall(triggers, guard, claim, decli,
          create.field_decl("vm_j", create.primitive_type(PrimitiveSort.Integer)),
          create.field_decl("vm_k", create.primitive_type(PrimitiveSort.Integer)),
          create.field_decl("vm_l", create.primitive_type(PrimitiveSort.Integer)));
    }
    if (type.isPrimitive(PrimitiveSort.Option)) {
      // show that rows are not none
      type = (Type) type.firstarg();
      res = star(res, create.forall(guardi, neq(rowi, create.reserved_name(ASTReserved.OptionNone)), decli));
      rowi = create.expression(StandardOperator.OptionGet, rowi);
    }
    // show that rows have specified length.
    if (!type.isPrimitive(PrimitiveSort.Array)) {
      Warning("Row of ValidMatrix is not an array");
    }
    res = star(res, create.forall(guardi, eq(create.expression(StandardOperator.Length, rowi), sz2), decli));
    if (mInjectivity != null) {
      res = star(res, mInjectivity);
    }
    return res;
  }
  
}
