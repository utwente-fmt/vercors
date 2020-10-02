package vct.col.util;

import hre.ast.Origin;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.CatchClause;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.decl.SignalsClause;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.java.ASTClassLoader;
import vct.logging.MessageFactory;
import vct.logging.PassAddVisitor;
import vct.logging.PassReport;
import vct.logging.VerCorsError;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * This class implements type checking of object oriented programs
 * that may use inheritance and/or overloading.
 * 
 * @author Stefan Blom
 *
 */
public class JavaTypeCheck extends AbstractTypeCheck {

  Set<Type> liveExceptionTypes = new HashSet<>();

  public JavaTypeCheck(PassReport report, ProgramUnit arg) {
    super(report, arg);
  }

  public void visit(Method m) {
    liveExceptionTypes = new HashSet<>();

    super.visit(m);

    if (m.getKind() == Method.Kind.Pure && m.canThrowSpec()) {
      Fail("Pure methods cannot throw exceptions");
    }

    for (Type t : m.signals) {
      if (!(t instanceof ClassType)) {
        Fail("Throws type can only be class");
      }

      ClassType ct = (ClassType) t;
      if (!isThrowableType(ct)) {
        Fail("Throws type must extend throwable");
      }

      // Remove throws type from the live exception types.
      // Any types leftover have to be unchecked!
      liveExceptionTypes.remove(t);
    }

    for (Type t : liveExceptionTypes) {
      if (isCheckedException(source(), t)) {
        reportFail(String.format("Cannot throw checked exception of type %s without adding it to the throws list", t),
                VerCorsError.ErrorCode.TypeError, VerCorsError.SubCode.UnlistedExceptionType, m);
      }
    }
  }

  public void visit(SignalsClause sc) {
    super.visit(sc);

    ClassType throwableType = new ClassType(ClassType.javaLangThrowableName());
    if (!isThrowableType(sc.type())) {
      reportFail("Signals type must extend Throwable",
              VerCorsError.ErrorCode.TypeError, VerCorsError.SubCode.ExtendsThrowable,
              sc.getType(), sc);
    }
  }

  public void reportFail(VerCorsError e) {
    if (report != null) {
      MessageFactory log=new MessageFactory(new PassAddVisitor(report));
      log.error(e);
    } else {
      Abort("Report was null: %s", e);
    }
  }

  public void reportFail(String message, VerCorsError.ErrorCode ec, VerCorsError.SubCode sc, ASTNode primaryOrigin, ASTNode... secondaryOrigins) {
    List<Origin> auxOrigin = Arrays.stream(secondaryOrigins).map(n -> n.getOrigin()).collect(Collectors.toList());
    reportFail(new VerCorsError(
            ec, sc,
            primaryOrigin.getOrigin(),
            auxOrigin
    ));
    Fail(message);
  }

  public void visit(TryCatchBlock tcb) {
    super.visit(tcb);

    // Types of catch clauses cannot overlap
    ArrayList<ClassType> encounteredCatchTypes = new ArrayList<>();
    ClassType throwableType = new ClassType(ClassType.javaLangThrowableName());

    for (CatchClause cc : tcb.catches()) {
      enter(cc);
      ArrayList<ClassType> encounteredMultiCatchTypes = new ArrayList<>();

      for (Type catchType : cc.javaCatchTypes()) {
        if (!(catchType instanceof ClassType)) {
          Fail("Catch clause types must be class types");
        }

        ClassType ct = (ClassType) catchType;

        if (!isThrowableType(ct)) {
          Fail("Catch clause types must inherit from Throwable");
        }

        for (ClassType t : encounteredMultiCatchTypes) {
          if (t.supertypeof(source(), ct)) {
            Fail("Types within a multi-catch cannot be subtypes");
          }
        }
        encounteredMultiCatchTypes.add(ct);

        for (Type t : encounteredCatchTypes) {
          if (t.supertypeof(source(), ct)) {
            Fail("Catch type %s is already caught by earlier catch clause", ct);
          }
        }
        encounteredCatchTypes.add(ct);
      }

      leave(cc);
    }
  }

  public static boolean isCheckedException(ProgramUnit source, Type t) {
    ClassType javaLangException = new ClassType(ClassType.javaLangExceptionName());
    ClassType javaLangRuntimeException = new ClassType(ClassType.javaLangRuntimeExceptionName());

    return !(t.equals(javaLangException))
            && javaLangException.supertypeof(source, t)
            && !javaLangRuntimeException.supertypeof(source, t);
  }

  public void visit(CatchClause cc) {
    for (Type t : cc.javaCatchTypes()) {
      // Unchecked exceptions are allowed even on empty try blocks
      if (!isCheckedException(source(), t)) {
        continue;
      }

      // If one of the catch types is a supertype of one of the live types,
      // the catch block is used, so no error
      boolean hasGuaranteedCatches = liveExceptionTypes.stream()
              .anyMatch(exceptionType -> t.supertypeof(source(), exceptionType));
      // If one of the live types is a supertype of the catch type,
      // the catch block might be used, so no error
      boolean hasPossibleCatches = liveExceptionTypes.stream().anyMatch(exceptionType -> exceptionType.supertypeof(source(), t));

      // If neither is the case, the catch block is unused, hence an error
      if (!(hasGuaranteedCatches || hasPossibleCatches)) {
        reportFail(String.format("Catch type %s is not thrown within corresponding try block", t),
                VerCorsError.ErrorCode.TypeError, VerCorsError.SubCode.UnusedCatch,
                t, cc
          );
      }

      // Remove exception types that are guaranteed to be caught from the live exceptions set
      liveExceptionTypes = liveExceptionTypes.stream()
              .filter(exceptionType -> !t.supertypeof(source(), exceptionType))
              .collect(Collectors.toSet());
    }

    super.visit(cc);
  }

  public void visit(ASTSpecial special) {
    super.visit(special);

    if (special.isSpecial(ASTSpecial.Kind.Throw)) {
      ASTNode throwee = special.getArg(0);
      if (!isThrowableType(throwee.getType())) {
        reportFail("Type of thrown expression needs to extend Throwable",
                VerCorsError.ErrorCode.TypeError, VerCorsError.SubCode.ExtendsThrowable,
                throwee, special);
      } else {
        liveExceptionTypes.add(throwee.getType());
      }
    }
  }

  private boolean isThrowableType(Type t) {
    if (t instanceof ClassType) {
      ASTClass astClass = (ASTClass)((ClassType) t).definitionJava(source(), ASTClassLoader.INSTANCE(), currentNamespace);
      if (astClass.kind == ASTClass.ClassKind.Record) {
        return true;
      }
    }

    ClassType throwableType = new ClassType(ClassType.javaLangThrowableName());
    ClassType unflatThrowableType = new ClassType(new String[]{"java", "lang", "Throwable"});
    visit(unflatThrowableType); // collect definition

    return throwableType.supertypeof(source(), t) // Actually throwable
            || unflatThrowableType.supertypeof(source(), t)
            || (t.toString().startsWith("__") && t.toString().endsWith("_ex")); // We defined it (sorry, hacky!)
    }

  public void visit(MethodInvokation mi) {
    super.visit(mi);

    if (mi.definition() != null && (mi.definition().getKind() == Method.Kind.Constructor || mi.definition().getKind() == Method.Kind.Plain)) {
      // Any types that the method has declared, can become live when calling this method
      for (Type t : mi.definition().signals) {
        liveExceptionTypes.add(t);
      }
    }
  }
}
