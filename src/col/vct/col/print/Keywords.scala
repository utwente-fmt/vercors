package vct.col.print

object Keywords {
  val SPEC: Set[String] = Set(
    "inline", "assert", "true", "false", "package", "resource", "process", "frac", "zfrac", "bool", "ref",
    "rational", "seq", "set", "bag", "pointer", "map", "option", "either", "tuple", "type", "any",
    "nothing", "string", "pure", "thread_local", "bip_annotation", "with", "then", "given", "yields",
    "axiom", "model", "adt", "prover_type", "prover_function", "modifies", "accessible", "requires",
    "ensures", "context_everywhere", "context", "loop_invariant", "kernel_invariant", "lock_invariant",
    "signals", "decreases", "apply", "fold", "unfold", "open", "close", "assume", "inhale", "exhale",
    "label", "extract", "frame", "outline", "refute", "witness", "ghost", "send", "to", "recv", "from",
    "transfer", "csl_subject", "spec_ignore", "action", "atomic", "commit", "Reducible", "AddsTo", "APerm",
    "ArrayPerm", "Contribution", "held", "committed", "HPerm", "idle", "perm", "Perm", "PointsTo",
    "running", "Some", "Left", "Right", "Value", "none", "None", "write", "read", "empty",
  )

  val PVL: Set[String] = Set(
    "inline", "assert", "package", "enum", "class", "seq_program", "seq_run", "kernel", "barrier",
    "invariant", "constructor", "run", "thread", "endpoint", "if", "else", "while", "for", "goto",
    "return", "vec", "par", "and", "parallel", "sequential", "block", "lock", "unlock", "wait", "notify",
    "fork", "join", "communicate", "this", "null", "true", "false", "current_thread", "global", "local",
    "static", "final", "unfolding", "in", "new", "id", "boolean", "void", "int", "char", "float32",
    "float64",
  )

  val JAVA: Set[String] = Set(
    "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const",
    "continue", "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for",
    "if", "goto", "implements", "import", "instanceof", "int", "interface", "long", "native", "new",
    "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super",
    "switch", "synchronized", "this", "throw", "throws", "transient", "try", "void", "volatile", "while",
  )

  val C_CPP_GPGPU: Set[String] = Set(
    "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic", "_Imaginary", "_Noreturn",
    "_Static_assert", "_Thread_local", "__cuda_kernel__", "__opencl_kernel__", "__vercors_atomic__",
    "__vercors_barrier__", "__vercors_global_mem_fence__", "__vercors_global_memory__",
    "__vercors_local_mem_fence__", "__vercors_local_memory__", "_thread_local", "alignas", "alignof",
    "asm", "auto", "bool", "break", "case", "catch", "char", "char16_t", "char32_t", "class", "const",
    "const_cast", "constexpr", "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast",
    "else", "enum", "explicit", "export", "extern", "false", "final", "float", "for", "friend", "goto",
    "if", "inline", "int", "long", "mutable", "namespace", "new", "noexcept", "nullptr", "operator",
    "override", "private", "protected", "public", "register", "reinterpret_cast", "restrict", "return",
    "short", "signed", "sizeof", "static", "static_assert", "static_cast", "struct", "switch", "template",
    "this", "throw", "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using",
    "virtual", "void", "volatile", "wchar_t", "while",
  )
}
