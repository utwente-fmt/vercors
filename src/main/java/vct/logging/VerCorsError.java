package vct.logging;

import java.util.ArrayList;
import hre.ast.Origin;
import viper.api.ViperError;
import static vct.logging.VerCorsError.ErrorCode.*;
import static vct.logging.VerCorsError.SubCode.*;

/**
 * Represents verifications errors as log messages.
 * 
 * @author Stefan Blom
 *
 */
public class VerCorsError extends AbstractMessage {

  /**
   * The verification problem found.
   * 
   * @author Stefan Blom
   *
   */
  public static enum ErrorCode {
    AssertFailed,
    ExhaleFailed,
    InvariantNotEstablished,
    InvariantNotPreserved,
    InvariantBroken,
    PostConditionFailed,
    UnspecifiedError,
    MagicWandUnproven,
    MagicWandPreCondition,
    NotWellFormed,
    ApplicationPreCondition,
    CallPreCondition,
    AssignmentFailed
  }
  
  /**
   * The reason for the verification problem.
   * 
   * @author Stefan Blom
   *
   */
  public static enum SubCode {
    AssertionFalse,
    DivisionByZero,
    InsufficientPermission,
    UnspecifiedCause
  }
  
  
  
  public final ErrorCode code;
  
  public final SubCode sub;
  
  public final Origin main;
  
  public final ArrayList<Origin> aux=new ArrayList<Origin>();
  
  
  public static VerCorsError viper_error(ViperError<Origin> e){
    ErrorCode code;
    SubCode sub;
    String err[]=e.getError(0).split(":");
    switch(err[0]){
    case "postcondition.violated":
      code=PostConditionFailed;
      break;
    case "exhale.failed":
      code=ExhaleFailed;
      break;
    case "assert.failed":
      code=AssertFailed;
      break;
    case "assignment.failed":
      code=AssignmentFailed;
      break;
    case "invariant.not.established":
      code=InvariantNotEstablished;
      break;
    case "invariant.not.preserved":
      code=InvariantNotPreserved;
      break;
    case "predicate.not.wellformed":
    case "not.wellformed":
      code=NotWellFormed;
      break;
    case "application.precondition":
      code=ApplicationPreCondition;
      break;
    case "call.precondition":
      code=CallPreCondition;
      break;
    default:
      hre.lang.System.Warning("unspecified error %s",err[0]);
      code=UnspecifiedError;
      break;
    }
    switch(err[1]){
    case "assertion.false":
      sub=AssertionFalse;
      break;
    case "division.by.zero":
      sub=DivisionByZero;
      break;
    case "insufficient.permission":
      sub=InsufficientPermission;
      break;
    case "receiver.not.injective":
      sub=InsufficientPermission;
      break;
    default:
      hre.lang.System.Warning("unspecified cause %s",err[1]);
      sub=UnspecifiedCause;
      break;    
    }
    Origin main=e.getOrigin(0);
    ArrayList<Origin> aux=new ArrayList<Origin>();
    for(int i=1;i<e.getExtraCount();i++){
      aux.add(e.getOrigin(i));
    }
    return new VerCorsError(code,sub,main,aux);
  }
  
  public VerCorsError(ErrorCode code, SubCode sub,Origin origin, ArrayList<Origin> aux) {
    this.code=code;
    this.sub=sub;
    this.main=origin;
    this.aux.addAll(aux);
    fatal=true;
  }

  @Override
  public void accept(MessageVisitor visitor){
    visitor.visit(this);
  }

}
