package vct.col.ast.type;

public enum ASTReserved {
  /**
   * Specification expression that represents the result of a method or function in a post condition. 
   */
  Result,
  /**
   * Refers to the super class of the current object.
   */
  Super,
  /**
   * Refers to the current object.
   */
  This,
  /**
   * Get the static class.
   */
  GetClass,
  /**
   * Denotes a public member of a class or module.
   */
  Public,
  /**
   * Denotes a private member of a class or module.
   * (For example the C keyword static maps to Private.)
   */
  Private,
  /**
   * Denotes a static member of a class.
   * All members of a module are static by default,
   * the C keyword static maps to Private.
   */
  Static,
  /**
   * volatile member
   */
  Volatile,
  /**
   * denotes a protected class or member.
   */
  Protected,
  /**
   * denotes an abstract class
   */
  Abstract,
  /**
   * Synchronized method modifier.
   */
  Synchronized,
  /**
   * null value for pointers.
   */
  Null,
  /**
   * Mark function/method/procedure/etc. as inline
   */
  Inline,
  /**
   * Limit argument to being a pure method.
   */
  Pure,
  /**
   * Declare function to be thread-local.
   */
  ThreadLocal,
  /**
   * any value specification value
   */
  Any,
  /**
   * Java final keyword.
   */
  Final,
  /**
   * Full Write Permission
   */
  FullPerm,
  /**
   * Read Permission
   */
  ReadPerm,
  /**
   * Zero Permission
   */
  NoPerm,
  /**
   * The new keyword is used to distinguish constructor calls from method calls.
   */
  New,
  /**
   * The empty process for history algebras.
   */
  EmptyProcess,
  /**
   * The current thread.
   */
  CurrentThread,
  /**
   * No value case for the option type.
   */
  OptionNone,
  /**
   * The default case in a switch.
   */
  Default,
  /**
   * Global thread ID, equivalent to get_global_id(0) in OpenCL.
   */
  GlobalThreadId,
  /**
   * Local thread ID, equivalent to get_local_id(0) in OpenCL.
   */
  LocalThreadId,
}

