; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_quantifier_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0

; Function Attrs: noinline nounwind uwtable
define dso_local void @foo(ptr noundef %0, i32 noundef %1) #0 !dbg !21 !pallas.fcontract !27 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !34, metadata !DIExpression()), !dbg !83
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !41, metadata !DIExpression()), !dbg !84
  %5 = load ptr, ptr %3, align 8, !dbg !85
  %6 = getelementptr inbounds i32, ptr %5, i64 0, !dbg !85
  store i32 0, ptr %6, align 4, !dbg !86
  %7 = load ptr, ptr %3, align 8, !dbg !87
  %8 = getelementptr inbounds i32, ptr %7, i64 1, !dbg !87
  store i32 2, ptr %8, align 4, !dbg !88
  ret void, !dbg !89
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !36 !pallas.exprWrapper !90 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !35, metadata !DIExpression()), !dbg !91
  call void @llvm.dbg.value(metadata i32 %1, metadata !42, metadata !DIExpression()), !dbg !91
  %3 = icmp ne ptr %0, null, !dbg !92
  br i1 %3, label %4, label %6, !dbg !93

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 2, !dbg !94
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !91
  ret i1 %7, !dbg !91
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !48 !pallas.exprWrapper !90 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !47, metadata !DIExpression()), !dbg !95
  call void @llvm.dbg.value(metadata i32 %1, metadata !50, metadata !DIExpression()), !dbg !95
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !96
  %4 = sext i32 %1 to i64, !dbg !97
  %5 = icmp eq i64 %3, %4, !dbg !98
  ret i1 %5, !dbg !95
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !56 !pallas.exprWrapper !90 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !55, metadata !DIExpression()), !dbg !99
  call void @llvm.dbg.value(metadata i32 %1, metadata !58, metadata !DIExpression()), !dbg !99
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !100
  %5 = icmp sle i32 0, %4, !dbg !101
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !102
  %7 = icmp slt i32 %6, %1, !dbg !103
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !104
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !105
  %10 = sext i32 %9 to i64, !dbg !106
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !106
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !107
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !108
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !109
  ret i1 %13, !dbg !99
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1) #0 !dbg !64 !pallas.exprWrapper !90 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !63, metadata !DIExpression()), !dbg !110
  call void @llvm.dbg.value(metadata i32 %1, metadata !66, metadata !DIExpression()), !dbg !110
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !111
  %4 = sext i32 %1 to i64, !dbg !112
  %5 = icmp eq i64 %3, %4, !dbg !113
  ret i1 %5, !dbg !110
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1) #0 !dbg !72 !pallas.exprWrapper !90 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !71, metadata !DIExpression()), !dbg !114
  call void @llvm.dbg.value(metadata i32 %1, metadata !74, metadata !DIExpression()), !dbg !114
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !115
  %5 = icmp sle i32 0, %4, !dbg !116
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !117
  %7 = icmp slt i32 %6, %1, !dbg !118
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !119
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !120
  %10 = sext i32 %9 to i64, !dbg !121
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !121
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !122
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !123
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !124
  ret i1 %13, !dbg !114
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1) #0 !dbg !80 !pallas.exprWrapper !90 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !79, metadata !DIExpression()), !dbg !125
  call void @llvm.dbg.value(metadata i32 %1, metadata !82, metadata !DIExpression()), !dbg !125
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !126
  %4 = icmp sle i32 0, %3, !dbg !127
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !128
  %6 = icmp slt i32 %5, %1, !dbg !129
  %7 = call i1 @pallas.scAnd(i1 %4, i1 %6), !dbg !130
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !131
  %9 = sext i32 %8 to i64, !dbg !132
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !132
  %11 = load i32, ptr %10, align 4, !dbg !132
  %12 = icmp eq i32 %11, 1, !dbg !133
  %13 = call i1 @pallas.exists(i1 %7, i1 %12), !dbg !134
  ret i1 %13, !dbg !125
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !135 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !136 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !137 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !138 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !139 i1 @pallas.exists(i1, i1)

declare !pallas.specLib !140 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !141 i32 @"pallas.boundVar i32"(ptr)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!7, !9}
!llvm.module.flags = !{!13, !14, !15, !16, !17, !18, !19}
!llvm.ident = !{!20, !20}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 31, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "c4347728266b8220c127e98aac6587fa")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = distinct !DICompileUnit(language: DW_LANG_C11, file: !8, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!8 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_quantifier_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "2ed1b2dfa6f8a0486441c1f41d556a0a")
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !2, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !10, globals: !12, splitDebugInlining: false, nameTableKind: None)
!10 = !{!11}
!11 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!12 = !{!0}
!13 = !{i32 7, !"Dwarf Version", i32 5}
!14 = !{i32 2, !"Debug Info Version", i32 3}
!15 = !{i32 1, !"wchar_size", i32 4}
!16 = !{i32 8, !"PIC Level", i32 2}
!17 = !{i32 7, !"PIE Level", i32 2}
!18 = !{i32 7, !"uwtable", i32 2}
!19 = !{i32 7, !"frame-pointer", i32 2}
!20 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!21 = distinct !DISubprogram(name: "foo", scope: !8, file: !8, line: 16, type: !22, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!22 = !DISubroutineType(types: !23)
!23 = !{null, !24, !25}
!24 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !25, size: 64)
!25 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!26 = !{}
!27 = !{!28, i1 false, i1 false, !26, !26, !30, !43, !51, !59, !67, !75}
!28 = !{!"pallas.srcLoc", i64 5, i64 1, i64 15, i64 1, !29}
!29 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_quantifier_fail.c", directory: "", checksumkind: CSK_MD5, checksum: "2ed1b2dfa6f8a0486441c1f41d556a0a")
!30 = !{!"pallas.requires", !31, ptr @PALLAS_SPEC_0, !26, !26, !32}
!31 = !{!"pallas.srcLoc", i64 6, i64 1, i64 6, i64 30, !29}
!32 = !{!33, !40}
!33 = !{!34, !35}
!34 = !DILocalVariable(name: "arr", arg: 1, scope: !21, file: !8, line: 16, type: !24)
!35 = !DILocalVariable(name: "arr", arg: 1, scope: !36, file: !8, line: 6, type: !24)
!36 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !8, file: !8, line: 6, type: !37, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!37 = !DISubroutineType(types: !38)
!38 = !{!39, !24, !25}
!39 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!40 = !{!41, !42}
!41 = !DILocalVariable(name: "n", arg: 2, scope: !21, file: !8, line: 16, type: !25)
!42 = !DILocalVariable(name: "n", arg: 2, scope: !36, file: !8, line: 6, type: !25)
!43 = !{!"pallas.requires", !44, ptr @PALLAS_SPEC_1, !26, !26, !45}
!44 = !{!"pallas.srcLoc", i64 7, i64 1, i64 7, i64 31, !29}
!45 = !{!46, !49}
!46 = !{!34, !47}
!47 = !DILocalVariable(name: "arr", arg: 1, scope: !48, file: !8, line: 7, type: !24)
!48 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !8, file: !8, line: 7, type: !37, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!49 = !{!41, !50}
!50 = !DILocalVariable(name: "n", arg: 2, scope: !48, file: !8, line: 7, type: !25)
!51 = !{!"pallas.requires", !52, ptr @PALLAS_SPEC_2, !26, !26, !53}
!52 = !{!"pallas.srcLoc", i64 8, i64 1, i64 9, i64 80, !29}
!53 = !{!54, !57}
!54 = !{!34, !55}
!55 = !DILocalVariable(name: "arr", arg: 1, scope: !56, file: !8, line: 8, type: !24)
!56 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !8, file: !8, line: 8, type: !37, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!57 = !{!41, !58}
!58 = !DILocalVariable(name: "n", arg: 2, scope: !56, file: !8, line: 8, type: !25)
!59 = !{!"pallas.ensures", !60, ptr @PALLAS_SPEC_3, !26, !26, !61}
!60 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 30, !29}
!61 = !{!62, !65}
!62 = !{!34, !63}
!63 = !DILocalVariable(name: "arr", arg: 1, scope: !64, file: !8, line: 10, type: !24)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !8, file: !8, line: 10, type: !37, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!65 = !{!41, !66}
!66 = !DILocalVariable(name: "n", arg: 2, scope: !64, file: !8, line: 10, type: !25)
!67 = !{!"pallas.ensures", !68, ptr @PALLAS_SPEC_4, !26, !26, !69}
!68 = !{!"pallas.srcLoc", i64 11, i64 1, i64 12, i64 79, !29}
!69 = !{!70, !73}
!70 = !{!34, !71}
!71 = !DILocalVariable(name: "arr", arg: 1, scope: !72, file: !8, line: 11, type: !24)
!72 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !8, file: !8, line: 11, type: !37, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!73 = !{!41, !74}
!74 = !DILocalVariable(name: "n", arg: 2, scope: !72, file: !8, line: 11, type: !25)
!75 = !{!"pallas.ensures", !76, ptr @PALLAS_SPEC_5, !26, !26, !77}
!76 = !{!"pallas.srcLoc", i64 13, i64 1, i64 14, i64 67, !29}
!77 = !{!78, !81}
!78 = !{!34, !79}
!79 = !DILocalVariable(name: "arr", arg: 1, scope: !80, file: !8, line: 13, type: !24)
!80 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !8, file: !8, line: 13, type: !37, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!81 = !{!41, !82}
!82 = !DILocalVariable(name: "n", arg: 2, scope: !80, file: !8, line: 13, type: !25)
!83 = !DILocation(line: 16, column: 15, scope: !21)
!84 = !DILocation(line: 16, column: 24, scope: !21)
!85 = !DILocation(line: 17, column: 5, scope: !21)
!86 = !DILocation(line: 17, column: 12, scope: !21)
!87 = !DILocation(line: 18, column: 5, scope: !21)
!88 = !DILocation(line: 18, column: 12, scope: !21)
!89 = !DILocation(line: 19, column: 1, scope: !21)
!90 = !{!""}
!91 = !DILocation(line: 0, scope: !36)
!92 = !DILocation(line: 6, column: 14, scope: !36)
!93 = !DILocation(line: 6, column: 22, scope: !36)
!94 = !DILocation(line: 6, column: 27, scope: !36)
!95 = !DILocation(line: 0, scope: !48)
!96 = !DILocation(line: 7, column: 10, scope: !48)
!97 = !DILocation(line: 7, column: 30, scope: !48)
!98 = !DILocation(line: 7, column: 27, scope: !48)
!99 = !DILocation(line: 0, scope: !56)
!100 = !DILocation(line: 8, column: 29, scope: !56)
!101 = !DILocation(line: 8, column: 26, scope: !56)
!102 = !DILocation(line: 9, column: 29, scope: !56)
!103 = !DILocation(line: 9, column: 41, scope: !56)
!104 = !DILocation(line: 8, column: 19, scope: !56)
!105 = !DILocation(line: 9, column: 58, scope: !56)
!106 = !DILocation(line: 9, column: 54, scope: !56)
!107 = !DILocation(line: 9, column: 72, scope: !56)
!108 = !DILocation(line: 9, column: 47, scope: !56)
!109 = !DILocation(line: 8, column: 10, scope: !56)
!110 = !DILocation(line: 0, scope: !64)
!111 = !DILocation(line: 10, column: 9, scope: !64)
!112 = !DILocation(line: 10, column: 29, scope: !64)
!113 = !DILocation(line: 10, column: 26, scope: !64)
!114 = !DILocation(line: 0, scope: !72)
!115 = !DILocation(line: 11, column: 28, scope: !72)
!116 = !DILocation(line: 11, column: 25, scope: !72)
!117 = !DILocation(line: 12, column: 28, scope: !72)
!118 = !DILocation(line: 12, column: 40, scope: !72)
!119 = !DILocation(line: 11, column: 18, scope: !72)
!120 = !DILocation(line: 12, column: 57, scope: !72)
!121 = !DILocation(line: 12, column: 53, scope: !72)
!122 = !DILocation(line: 12, column: 71, scope: !72)
!123 = !DILocation(line: 12, column: 46, scope: !72)
!124 = !DILocation(line: 11, column: 9, scope: !72)
!125 = !DILocation(line: 0, scope: !80)
!126 = !DILocation(line: 13, column: 27, scope: !80)
!127 = !DILocation(line: 13, column: 24, scope: !80)
!128 = !DILocation(line: 14, column: 27, scope: !80)
!129 = !DILocation(line: 14, column: 39, scope: !80)
!130 = !DILocation(line: 13, column: 17, scope: !80)
!131 = !DILocation(line: 14, column: 49, scope: !80)
!132 = !DILocation(line: 14, column: 45, scope: !80)
!133 = !DILocation(line: 14, column: 62, scope: !80)
!134 = !DILocation(line: 13, column: 9, scope: !80)
!135 = !{!"pallas.ptrLength"}
!136 = !{!"pallas.forallSep"}
!137 = !{!"pallas.perm"}
!138 = !{!"pallas.fracOf"}
!139 = !{!"pallas.exists"}
!140 = !{!"pallas.scAnd"}
!141 = !{!"pallas.boundVar"}
