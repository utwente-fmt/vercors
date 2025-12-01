; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_pred.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [12 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @arrWrite, ptr @arrZero], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0
@.str.1 = private unnamed_addr constant [2 x i8] c"j\00", align 1, !dbg !7

; Function Attrs: noinline nounwind uwtable
define dso_local void @zero_arr(ptr noundef %0, i32 noundef %1) #0 !dbg !26 !pallas.fcontract !32 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !37, metadata !DIExpression()), !dbg !43
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !38, metadata !DIExpression()), !dbg !44
  call void @llvm.dbg.declare(metadata ptr %5, metadata !45, metadata !DIExpression()), !dbg !47
  store i32 0, ptr %5, align 4, !dbg !47
  br label %6, !dbg !48

6:                                                ; preds = %15, %2
  %7 = load i32, ptr %5, align 4, !dbg !49
  %8 = load i32, ptr %4, align 4, !dbg !51
  %9 = icmp slt i32 %7, %8, !dbg !52
  br i1 %9, label %10, label %18, !dbg !53

10:                                               ; preds = %6
  %11 = load ptr, ptr %3, align 8, !dbg !54, !pallas.stmntBlock !56
  %12 = load i32, ptr %5, align 4, !dbg !60
  %13 = sext i32 %12 to i64, !dbg !54
  %14 = getelementptr inbounds i32, ptr %11, i64 %13, !dbg !54
  store i32 0, ptr %14, align 4, !dbg !61
  br label %15, !dbg !62, !pallas.stmntBlock !63

15:                                               ; preds = %10
  %16 = load i32, ptr %5, align 4, !dbg !67
  %17 = add nsw i32 %16, 1, !dbg !67
  store i32 %17, ptr %5, align 4, !dbg !67
  br label %6, !dbg !68, !llvm.loop !69

18:                                               ; preds = %6
  ret void, !dbg !80, !pallas.stmntBlock !81
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !87 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !92, metadata !DIExpression()), !dbg !93
  call void @llvm.dbg.value(metadata i32 %1, metadata !94, metadata !DIExpression()), !dbg !93
  %3 = icmp sge i32 %1, 0, !dbg !95
  ret i1 %3, !dbg !93
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !96 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !97, metadata !DIExpression()), !dbg !98
  call void @llvm.dbg.value(metadata i32 %1, metadata !99, metadata !DIExpression()), !dbg !98
  %3 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !100
  ret i1 %3, !dbg !98
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !101 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !102, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i32 %1, metadata !104, metadata !DIExpression()), !dbg !103
  %3 = call zeroext i1 @arrZero(ptr noundef %0, i32 noundef %1), !dbg !105
  ret i1 %3, !dbg !103
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !106 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !109, metadata !DIExpression()), !dbg !110
  call void @llvm.dbg.value(metadata i32 %1, metadata !111, metadata !DIExpression()), !dbg !110
  call void @llvm.dbg.value(metadata i32 %2, metadata !112, metadata !DIExpression()), !dbg !110
  %4 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !113
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !114
  %6 = icmp sle i32 0, %5, !dbg !114
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !114
  %8 = icmp slt i32 %7, %2, !dbg !114
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !114
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !115
  %11 = sext i32 %10 to i64, !dbg !116
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !116
  %13 = load i32, ptr %12, align 4, !dbg !116
  %14 = icmp eq i32 %13, 0, !dbg !117
  %15 = call i1 @pallas.forall(i1 %9, i1 %14), !dbg !118
  %16 = call zeroext i1 @"pallas.unfolding zeroext i1_noundef zeroext i1"(i1 %4, i1 noundef zeroext %15), !dbg !119
  ret i1 %16, !dbg !110
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !120 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !121, metadata !DIExpression()), !dbg !122
  call void @llvm.dbg.value(metadata i32 %1, metadata !123, metadata !DIExpression()), !dbg !122
  call void @llvm.dbg.value(metadata i32 %2, metadata !124, metadata !DIExpression()), !dbg !122
  %4 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !125
  ret i1 %4, !dbg !122
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !126 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !127, metadata !DIExpression()), !dbg !128
  call void @llvm.dbg.value(metadata i32 %1, metadata !129, metadata !DIExpression()), !dbg !128
  call void @llvm.dbg.value(metadata i32 %2, metadata !130, metadata !DIExpression()), !dbg !128
  %4 = icmp sle i32 0, %2, !dbg !131
  br i1 %4, label %5, label %7, !dbg !132

5:                                                ; preds = %3
  %6 = icmp sle i32 %2, %1, !dbg !133
  br label %7

7:                                                ; preds = %5, %3
  %8 = phi i1 [ false, %3 ], [ %6, %5 ], !dbg !128
  ret i1 %8, !dbg !128
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !134 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !135, metadata !DIExpression()), !dbg !136
  call void @llvm.dbg.value(metadata i32 %1, metadata !137, metadata !DIExpression()), !dbg !136
  call void @llvm.dbg.value(metadata i32 %2, metadata !138, metadata !DIExpression()), !dbg !136
  %4 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !139
  ret i1 %4, !dbg !136
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !140 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !141, metadata !DIExpression()), !dbg !142
  call void @llvm.dbg.value(metadata i32 %1, metadata !143, metadata !DIExpression()), !dbg !142
  call void @llvm.dbg.value(metadata i32 %2, metadata !144, metadata !DIExpression()), !dbg !142
  %4 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !145
  ret i1 %4, !dbg !142
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1) #0 !dbg !146 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !147, metadata !DIExpression()), !dbg !148
  call void @llvm.dbg.value(metadata i32 %1, metadata !149, metadata !DIExpression()), !dbg !148
  %3 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !150
  ret i1 %3, !dbg !148
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0, i32 noundef %1) #0 !dbg !151 !pallas.exprWrapper !91 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !152, metadata !DIExpression()), !dbg !153
  call void @llvm.dbg.value(metadata i32 %1, metadata !154, metadata !DIExpression()), !dbg !153
  %3 = call zeroext i1 @arrZero(ptr noundef %0, i32 noundef %1), !dbg !155
  ret i1 %3, !dbg !153
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1) #0 !dbg !156 !pallas.predDef !157 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !158, metadata !DIExpression()), !dbg !159
  call void @llvm.dbg.value(metadata i32 %1, metadata !160, metadata !DIExpression()), !dbg !159
  %4 = icmp ne ptr %0, null, !dbg !161
  br i1 %4, label %5, label %20, !dbg !162

5:                                                ; preds = %2
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !163
  %7 = sext i32 %1 to i64, !dbg !164
  %8 = icmp sge i64 %6, %7, !dbg !165
  br i1 %8, label %9, label %20, !dbg !166

9:                                                ; preds = %5
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !167
  %11 = icmp sle i32 0, %10, !dbg !167
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !167
  %13 = icmp slt i32 %12, %1, !dbg !167
  %14 = call i1 @pallas.scAnd(i1 %11, i1 %13), !dbg !167
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !168
  %16 = sext i32 %15 to i64, !dbg !169
  %17 = getelementptr inbounds i32, ptr %0, i64 %16, !dbg !169
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !170
  %18 = call i1 @pallas.perm(ptr noundef %17, ptr noundef byval(%pallas.fracT) %3), !dbg !171
  %19 = call i1 @pallas.forallSep(i1 %14, i1 %18), !dbg !172
  br label %20

20:                                               ; preds = %9, %5, %2
  %21 = phi i1 [ false, %5 ], [ false, %2 ], [ %19, %9 ], !dbg !159
  ret i1 %21, !dbg !159
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @arrZero(ptr noundef %0, i32 noundef %1) #0 !dbg !173 !pallas.predDef !157 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !174, metadata !DIExpression()), !dbg !175
  call void @llvm.dbg.value(metadata i32 %1, metadata !176, metadata !DIExpression()), !dbg !175
  %4 = icmp ne ptr %0, null, !dbg !177
  br i1 %4, label %5, label %32, !dbg !178

5:                                                ; preds = %2
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !179
  %7 = sext i32 %1 to i64, !dbg !180
  %8 = icmp sge i64 %6, %7, !dbg !181
  br i1 %8, label %9, label %32, !dbg !182

9:                                                ; preds = %5
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !183
  %11 = icmp sle i32 0, %10, !dbg !183
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !183
  %13 = icmp slt i32 %12, %1, !dbg !183
  %14 = call i1 @pallas.scAnd(i1 %11, i1 %13), !dbg !183
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !184
  %16 = sext i32 %15 to i64, !dbg !185
  %17 = getelementptr inbounds i32, ptr %0, i64 %16, !dbg !185
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !186
  %18 = call i1 @pallas.perm(ptr noundef %17, ptr noundef byval(%pallas.fracT) %3), !dbg !187
  %19 = call i1 @pallas.forallSep(i1 %14, i1 %18), !dbg !188
  %20 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !189
  %21 = icmp sle i32 0, %20, !dbg !189
  %22 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !189
  %23 = icmp slt i32 %22, %1, !dbg !189
  %24 = call i1 @pallas.scAnd(i1 %21, i1 %23), !dbg !189
  %25 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !190
  %26 = sext i32 %25 to i64, !dbg !191
  %27 = getelementptr inbounds i32, ptr %0, i64 %26, !dbg !191
  %28 = load i32, ptr %27, align 4, !dbg !191
  %29 = icmp eq i32 %28, 0, !dbg !192
  %30 = call i1 @pallas.forall(i1 %24, i1 %29), !dbg !193
  %31 = call i1 @pallas.sepConj(i1 %19, i1 %30), !dbg !194
  br label %32

32:                                               ; preds = %9, %5, %2
  %33 = phi i1 [ false, %5 ], [ false, %2 ], [ %31, %9 ], !dbg !175
  ret i1 %33, !dbg !175
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !195 zeroext i1 @"pallas.unfolding zeroext i1_noundef zeroext i1"(i1 noundef zeroext, i1 noundef zeroext)

declare !pallas.specLib !196 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !197 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !198 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !199 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !200 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !201 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !202 i32 @"pallas.boundVar i32"(ptr)

declare !pallas.specLib !203 i64 @pallas.ptrLength(ptr noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!9, !11, !15, !17}
!llvm.module.flags = !{!18, !19, !20, !21, !22, !23, !24}
!llvm.ident = !{!25, !25}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 22, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "9322c24abdc2a7866283a11301456819")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 87, type: !3, isLocal: true, isDefinition: true)
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !10, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_pred.c", directory: ".", checksumkind: CSK_MD5, checksum: "a7cbc659c62e94ae2a1b91e00a1ab5ca")
!11 = distinct !DICompileUnit(language: DW_LANG_C11, file: !2, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !12, globals: !14, splitDebugInlining: false, nameTableKind: None)
!12 = !{!13}
!13 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!14 = !{!0, !7}
!15 = distinct !DICompileUnit(language: DW_LANG_C, file: !16, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!16 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_pred.c", directory: "")
!17 = distinct !DICompileUnit(language: DW_LANG_C, file: !16, producer: "spectral", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!18 = !{i32 7, !"Dwarf Version", i32 5}
!19 = !{i32 2, !"Debug Info Version", i32 3}
!20 = !{i32 1, !"wchar_size", i32 4}
!21 = !{i32 8, !"PIC Level", i32 2}
!22 = !{i32 7, !"PIE Level", i32 2}
!23 = !{i32 7, !"uwtable", i32 2}
!24 = !{i32 7, !"frame-pointer", i32 2}
!25 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!26 = distinct !DISubprogram(name: "zero_arr", scope: !10, file: !10, line: 28, type: !27, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!27 = !DISubroutineType(types: !28)
!28 = !{null, !29, !30}
!29 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !30, size: 64)
!30 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!31 = !{}
!32 = !{!33, i1 false, i1 false, !35, !39, !41}
!33 = !{!"pallas.srcLoc", i64 23, i64 1, i64 27, i64 1, !34}
!34 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_pred.c", directory: "", checksumkind: CSK_MD5, checksum: "a7cbc659c62e94ae2a1b91e00a1ab5ca")
!35 = !{!"pallas.requires", !36, ptr @PALLAS_SPEC_0, !37, !38}
!36 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 19, !34}
!37 = !DILocalVariable(name: "arr", arg: 1, scope: !26, file: !10, line: 28, type: !29)
!38 = !DILocalVariable(name: "size", arg: 2, scope: !26, file: !10, line: 28, type: !30)
!39 = !{!"pallas.requires", !40, ptr @PALLAS_SPEC_1, !37, !38}
!40 = !{!"pallas.srcLoc", i64 25, i64 1, i64 25, i64 29, !34}
!41 = !{!"pallas.ensures", !42, ptr @PALLAS_SPEC_2, !37, !38}
!42 = !{!"pallas.srcLoc", i64 26, i64 1, i64 26, i64 27, !34}
!43 = !DILocation(line: 28, column: 20, scope: !26)
!44 = !DILocation(line: 28, column: 29, scope: !26)
!45 = !DILocalVariable(name: "i", scope: !46, file: !10, line: 36, type: !30)
!46 = distinct !DILexicalBlock(scope: !26, file: !10, line: 36, column: 5)
!47 = !DILocation(line: 36, column: 14, scope: !46)
!48 = !DILocation(line: 36, column: 10, scope: !46)
!49 = !DILocation(line: 36, column: 21, scope: !50)
!50 = distinct !DILexicalBlock(scope: !46, file: !10, line: 36, column: 5)
!51 = !DILocation(line: 36, column: 25, scope: !50)
!52 = !DILocation(line: 36, column: 23, scope: !50)
!53 = !DILocation(line: 36, column: 5, scope: !46)
!54 = !DILocation(line: 40, column: 9, scope: !55)
!55 = distinct !DILexicalBlock(scope: !50, file: !10, line: 36, column: 36)
!56 = !{!57, !58}
!57 = !{!"pallas.srcLoc", i64 37, i64 9, i64 39, i64 9, !34}
!58 = !{!"pallas.unfold", !59, ptr @PALLAS_SPEC_6, !37, !38, !45}
!59 = !{!"pallas.srcLoc", i64 38, i64 9, i64 38, i64 35, !34}
!60 = !DILocation(line: 40, column: 13, scope: !55)
!61 = !DILocation(line: 40, column: 16, scope: !55)
!62 = !DILocation(line: 44, column: 5, scope: !55)
!63 = !{!64, !65}
!64 = !{!"pallas.srcLoc", i64 41, i64 9, i64 43, i64 9, !34}
!65 = !{!"pallas.fold", !66, ptr @PALLAS_SPEC_7, !37, !38, !45}
!66 = !{!"pallas.srcLoc", i64 42, i64 9, i64 42, i64 33, !34}
!67 = !DILocation(line: 36, column: 31, scope: !50)
!68 = !DILocation(line: 36, column: 5, scope: !50)
!69 = distinct !{!69, !53, !70, !71, !72}
!70 = !DILocation(line: 44, column: 5, scope: !46)
!71 = !{!"llvm.loop.mustprogress"}
!72 = !{!"pallas.loopInv", !73, !74, !76, !78}
!73 = !{!"pallas.srcLoc", i64 29, i64 5, i64 35, i64 5, !34}
!74 = !{!75, ptr @PALLAS_SPEC_3, !37, !38, !45}
!75 = !{!"pallas.srcLoc", i64 30, i64 5, i64 30, i64 39, !34}
!76 = !{!77, ptr @PALLAS_SPEC_4, !37, !38, !45}
!77 = !{!"pallas.srcLoc", i64 31, i64 5, i64 31, i64 39, !34}
!78 = !{!79, ptr @PALLAS_SPEC_5, !37, !38, !45}
!79 = !{!"pallas.srcLoc", i64 32, i64 5, i64 34, i64 68, !34}
!80 = !DILocation(line: 50, column: 1, scope: !26)
!81 = !{!82, !83, !85}
!82 = !{!"pallas.srcLoc", i64 46, i64 5, i64 49, i64 5, !34}
!83 = !{!"pallas.unfold", !84, ptr @PALLAS_SPEC_8, !37, !38}
!84 = !{!"pallas.srcLoc", i64 47, i64 5, i64 47, i64 31, !34}
!85 = !{!"pallas.fold", !86, ptr @PALLAS_SPEC_9, !37, !38}
!86 = !{!"pallas.srcLoc", i64 48, i64 5, i64 48, i64 28, !34}
!87 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !10, file: !10, line: 24, type: !88, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!88 = !DISubroutineType(types: !89)
!89 = !{!90, !29, !30}
!90 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!91 = !{!""}
!92 = !DILocalVariable(name: "arr", arg: 1, scope: !87, file: !10, line: 24, type: !29)
!93 = !DILocation(line: 0, scope: !87)
!94 = !DILocalVariable(name: "size", arg: 2, scope: !87, file: !10, line: 24, type: !30)
!95 = !DILocation(line: 24, column: 15, scope: !87)
!96 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !10, file: !10, line: 25, type: !88, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!97 = !DILocalVariable(name: "arr", arg: 1, scope: !96, file: !10, line: 25, type: !29)
!98 = !DILocation(line: 0, scope: !96)
!99 = !DILocalVariable(name: "size", arg: 2, scope: !96, file: !10, line: 25, type: !30)
!100 = !DILocation(line: 25, column: 10, scope: !96)
!101 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !10, file: !10, line: 26, type: !88, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!102 = !DILocalVariable(name: "arr", arg: 1, scope: !101, file: !10, line: 26, type: !29)
!103 = !DILocation(line: 0, scope: !101)
!104 = !DILocalVariable(name: "size", arg: 2, scope: !101, file: !10, line: 26, type: !30)
!105 = !DILocation(line: 26, column: 9, scope: !101)
!106 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !10, file: !10, line: 32, type: !107, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!107 = !DISubroutineType(types: !108)
!108 = !{!90, !29, !30, !30}
!109 = !DILocalVariable(name: "arr", arg: 1, scope: !106, file: !10, line: 32, type: !29)
!110 = !DILocation(line: 0, scope: !106)
!111 = !DILocalVariable(name: "size", arg: 2, scope: !106, file: !10, line: 32, type: !30)
!112 = !DILocalVariable(name: "i", arg: 3, scope: !106, file: !10, line: 32, type: !30)
!113 = !DILocation(line: 32, column: 37, scope: !106)
!114 = !DILocation(line: 33, column: 33, scope: !106)
!115 = !DILocation(line: 34, column: 49, scope: !106)
!116 = !DILocation(line: 34, column: 45, scope: !106)
!117 = !DILocation(line: 34, column: 62, scope: !106)
!118 = !DILocation(line: 33, column: 25, scope: !106)
!119 = !DILocation(line: 32, column: 20, scope: !106)
!120 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !10, file: !10, line: 31, type: !107, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!121 = !DILocalVariable(name: "arr", arg: 1, scope: !120, file: !10, line: 31, type: !29)
!122 = !DILocation(line: 0, scope: !120)
!123 = !DILocalVariable(name: "size", arg: 2, scope: !120, file: !10, line: 31, type: !30)
!124 = !DILocalVariable(name: "i", arg: 3, scope: !120, file: !10, line: 31, type: !30)
!125 = !DILocation(line: 31, column: 20, scope: !120)
!126 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !10, file: !10, line: 30, type: !107, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!127 = !DILocalVariable(name: "arr", arg: 1, scope: !126, file: !10, line: 30, type: !29)
!128 = !DILocation(line: 0, scope: !126)
!129 = !DILocalVariable(name: "size", arg: 2, scope: !126, file: !10, line: 30, type: !30)
!130 = !DILocalVariable(name: "i", arg: 3, scope: !126, file: !10, line: 30, type: !30)
!131 = !DILocation(line: 30, column: 22, scope: !126)
!132 = !DILocation(line: 30, column: 27, scope: !126)
!133 = !DILocation(line: 30, column: 32, scope: !126)
!134 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !10, file: !10, line: 38, type: !107, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!135 = !DILocalVariable(name: "arr", arg: 1, scope: !134, file: !10, line: 38, type: !29)
!136 = !DILocation(line: 0, scope: !134)
!137 = !DILocalVariable(name: "size", arg: 2, scope: !134, file: !10, line: 38, type: !30)
!138 = !DILocalVariable(name: "i", arg: 3, scope: !134, file: !10, line: 38, type: !30)
!139 = !DILocation(line: 38, column: 16, scope: !134)
!140 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !10, file: !10, line: 42, type: !107, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!141 = !DILocalVariable(name: "arr", arg: 1, scope: !140, file: !10, line: 42, type: !29)
!142 = !DILocation(line: 0, scope: !140)
!143 = !DILocalVariable(name: "size", arg: 2, scope: !140, file: !10, line: 42, type: !30)
!144 = !DILocalVariable(name: "i", arg: 3, scope: !140, file: !10, line: 42, type: !30)
!145 = !DILocation(line: 42, column: 14, scope: !140)
!146 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !10, file: !10, line: 47, type: !88, scopeLine: 47, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!147 = !DILocalVariable(name: "arr", arg: 1, scope: !146, file: !10, line: 47, type: !29)
!148 = !DILocation(line: 0, scope: !146)
!149 = !DILocalVariable(name: "size", arg: 2, scope: !146, file: !10, line: 47, type: !30)
!150 = !DILocation(line: 47, column: 12, scope: !146)
!151 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !10, file: !10, line: 48, type: !88, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!152 = !DILocalVariable(name: "arr", arg: 1, scope: !151, file: !10, line: 48, type: !29)
!153 = !DILocation(line: 0, scope: !151)
!154 = !DILocalVariable(name: "size", arg: 2, scope: !151, file: !10, line: 48, type: !30)
!155 = !DILocation(line: 48, column: 10, scope: !151)
!156 = distinct !DISubprogram(name: "arrWrite", scope: !16, file: !16, line: 11, type: !88, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !15, retainedNodes: !31)
!157 = !{i1 false}
!158 = !DILocalVariable(name: "a", arg: 1, scope: !156, file: !16, line: 11, type: !29)
!159 = !DILocation(line: 0, scope: !156)
!160 = !DILocalVariable(name: "n", arg: 2, scope: !156, file: !16, line: 11, type: !30)
!161 = !DILocation(line: 11, column: 40, scope: !156)
!162 = !DILocation(line: 11, column: 48, scope: !156)
!163 = !DILocation(line: 12, column: 38, scope: !156)
!164 = !DILocation(line: 12, column: 56, scope: !156)
!165 = !DILocation(line: 12, column: 53, scope: !156)
!166 = !DILocation(line: 12, column: 58, scope: !156)
!167 = !DILocation(line: 13, column: 47, scope: !156)
!168 = !DILocation(line: 14, column: 56, scope: !156)
!169 = !DILocation(line: 14, column: 54, scope: !156)
!170 = !DILocation(line: 14, column: 70, scope: !156)
!171 = !DILocation(line: 14, column: 47, scope: !156)
!172 = !DILocation(line: 13, column: 38, scope: !156)
!173 = distinct !DISubprogram(name: "arrZero", scope: !16, file: !16, line: 15, type: !88, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !31)
!174 = !DILocalVariable(name: "a", arg: 1, scope: !173, file: !16, line: 15, type: !29)
!175 = !DILocation(line: 0, scope: !173)
!176 = !DILocalVariable(name: "n", arg: 2, scope: !173, file: !16, line: 15, type: !30)
!177 = !DILocation(line: 15, column: 39, scope: !173)
!178 = !DILocation(line: 15, column: 47, scope: !173)
!179 = !DILocation(line: 16, column: 37, scope: !173)
!180 = !DILocation(line: 16, column: 55, scope: !173)
!181 = !DILocation(line: 16, column: 52, scope: !173)
!182 = !DILocation(line: 16, column: 57, scope: !173)
!183 = !DILocation(line: 17, column: 51, scope: !173)
!184 = !DILocation(line: 18, column: 60, scope: !173)
!185 = !DILocation(line: 18, column: 58, scope: !173)
!186 = !DILocation(line: 18, column: 74, scope: !173)
!187 = !DILocation(line: 18, column: 51, scope: !173)
!188 = !DILocation(line: 17, column: 42, scope: !173)
!189 = !DILocation(line: 19, column: 50, scope: !173)
!190 = !DILocation(line: 20, column: 52, scope: !173)
!191 = !DILocation(line: 20, column: 50, scope: !173)
!192 = !DILocation(line: 20, column: 65, scope: !173)
!193 = !DILocation(line: 19, column: 42, scope: !173)
!194 = !DILocation(line: 17, column: 37, scope: !173)
!195 = !{!"pallas.unfolding"}
!196 = !{!"pallas.sepConj"}
!197 = !{!"pallas.forall"}
!198 = !{!"pallas.forallSep"}
!199 = !{!"pallas.perm"}
!200 = !{!"pallas.fracOf"}
!201 = !{!"pallas.scAnd"}
!202 = !{!"pallas.boundVar"}
!203 = !{!"pallas.ptrLength"}
