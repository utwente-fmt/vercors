; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_pred.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [12 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @arrWrite, ptr @arrZero], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0
@.str.1 = private unnamed_addr constant [2 x i8] c"j\00", align 1, !dbg !7

; Function Attrs: noinline nounwind uwtable
define dso_local void @zero_arr(ptr noundef %0, i32 noundef %1) #0 !dbg !26 !pallas.fcontract !32 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !39, metadata !DIExpression()), !dbg !64
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !46, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.declare(metadata ptr %5, metadata !66, metadata !DIExpression()), !dbg !68
  store i32 0, ptr %5, align 4, !dbg !68
  br label %6, !dbg !69

6:                                                ; preds = %15, %2
  %7 = load i32, ptr %5, align 4, !dbg !70
  %8 = load i32, ptr %4, align 4, !dbg !72
  %9 = icmp slt i32 %7, %8, !dbg !73
  br i1 %9, label %10, label %18, !dbg !74

10:                                               ; preds = %6
  %11 = load ptr, ptr %3, align 8, !dbg !75, !pallas.stmntBlock !77
  %12 = load i32, ptr %5, align 4, !dbg !91
  %13 = sext i32 %12 to i64, !dbg !75
  %14 = getelementptr inbounds i32, ptr %11, i64 %13, !dbg !75
  store i32 0, ptr %14, align 4, !dbg !92
  br label %15, !dbg !93, !pallas.stmntBlock !94

15:                                               ; preds = %10
  %16 = load i32, ptr %5, align 4, !dbg !106
  %17 = add nsw i32 %16, 1, !dbg !106
  store i32 %17, ptr %5, align 4, !dbg !106
  br label %6, !dbg !107, !llvm.loop !108

18:                                               ; preds = %6
  ret void, !dbg !143, !pallas.stmntBlock !144
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !41 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !40, metadata !DIExpression()), !dbg !163
  call void @llvm.dbg.value(metadata i32 %1, metadata !47, metadata !DIExpression()), !dbg !163
  %3 = icmp sge i32 %1, 0, !dbg !164
  ret i1 %3, !dbg !163
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !53 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !52, metadata !DIExpression()), !dbg !165
  call void @llvm.dbg.value(metadata i32 %1, metadata !55, metadata !DIExpression()), !dbg !165
  %3 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !166
  ret i1 %3, !dbg !165
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !61 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !60, metadata !DIExpression()), !dbg !167
  call void @llvm.dbg.value(metadata i32 %1, metadata !63, metadata !DIExpression()), !dbg !167
  %3 = call zeroext i1 @arrZero(ptr noundef %0, i32 noundef %1), !dbg !168
  ret i1 %3, !dbg !167
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !128 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !127, metadata !DIExpression()), !dbg !169
  call void @llvm.dbg.value(metadata i32 %1, metadata !130, metadata !DIExpression()), !dbg !169
  call void @llvm.dbg.value(metadata i32 %2, metadata !132, metadata !DIExpression()), !dbg !169
  %4 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !170
  ret i1 %4, !dbg !169
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !118 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !117, metadata !DIExpression()), !dbg !171
  call void @llvm.dbg.value(metadata i32 %1, metadata !120, metadata !DIExpression()), !dbg !171
  call void @llvm.dbg.value(metadata i32 %2, metadata !122, metadata !DIExpression()), !dbg !171
  %4 = icmp sle i32 0, %2, !dbg !172
  br i1 %4, label %5, label %7, !dbg !173

5:                                                ; preds = %3
  %6 = icmp sle i32 %2, %1, !dbg !174
  br label %7

7:                                                ; preds = %5, %3
  %8 = phi i1 [ false, %3 ], [ %6, %5 ], !dbg !171
  ret i1 %8, !dbg !171
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !138 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !137, metadata !DIExpression()), !dbg !175
  call void @llvm.dbg.value(metadata i32 %1, metadata !140, metadata !DIExpression()), !dbg !175
  call void @llvm.dbg.value(metadata i32 %2, metadata !142, metadata !DIExpression()), !dbg !175
  %4 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !176
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !177
  %6 = icmp sle i32 0, %5, !dbg !177
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !177
  %8 = icmp slt i32 %7, %2, !dbg !177
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !177
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str.1), !dbg !178
  %11 = sext i32 %10 to i64, !dbg !179
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !179
  %13 = load i32, ptr %12, align 4, !dbg !179
  %14 = icmp eq i32 %13, 0, !dbg !180
  %15 = call i1 @pallas.forall(i1 %9, i1 %14), !dbg !181
  %16 = call zeroext i1 @"pallas.unfolding zeroext i1_noundef zeroext i1"(i1 %4, i1 noundef zeroext %15), !dbg !182
  ret i1 %16, !dbg !175
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !84 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !83, metadata !DIExpression()), !dbg !183
  call void @llvm.dbg.value(metadata i32 %1, metadata !88, metadata !DIExpression()), !dbg !183
  call void @llvm.dbg.value(metadata i32 %2, metadata !90, metadata !DIExpression()), !dbg !183
  %4 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !184
  ret i1 %4, !dbg !183
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !101 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !100, metadata !DIExpression()), !dbg !185
  call void @llvm.dbg.value(metadata i32 %1, metadata !103, metadata !DIExpression()), !dbg !185
  call void @llvm.dbg.value(metadata i32 %2, metadata !105, metadata !DIExpression()), !dbg !185
  %4 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !186
  ret i1 %4, !dbg !185
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1) #0 !dbg !151 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !150, metadata !DIExpression()), !dbg !187
  call void @llvm.dbg.value(metadata i32 %1, metadata !153, metadata !DIExpression()), !dbg !187
  %3 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !188
  ret i1 %3, !dbg !187
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0, i32 noundef %1) #0 !dbg !159 !pallas.exprWrapper !162 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !158, metadata !DIExpression()), !dbg !189
  call void @llvm.dbg.value(metadata i32 %1, metadata !161, metadata !DIExpression()), !dbg !189
  %3 = call zeroext i1 @arrZero(ptr noundef %0, i32 noundef %1), !dbg !190
  ret i1 %3, !dbg !189
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1) #0 !dbg !191 !pallas.predDef !192 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !193, metadata !DIExpression()), !dbg !194
  call void @llvm.dbg.value(metadata i32 %1, metadata !195, metadata !DIExpression()), !dbg !194
  %4 = icmp ne ptr %0, null, !dbg !196
  br i1 %4, label %5, label %20, !dbg !197

5:                                                ; preds = %2
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !198
  %7 = sext i32 %1 to i64, !dbg !199
  %8 = icmp sge i64 %6, %7, !dbg !200
  br i1 %8, label %9, label %20, !dbg !201

9:                                                ; preds = %5
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !202
  %11 = icmp sle i32 0, %10, !dbg !202
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !202
  %13 = icmp slt i32 %12, %1, !dbg !202
  %14 = call i1 @pallas.scAnd(i1 %11, i1 %13), !dbg !202
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !203
  %16 = sext i32 %15 to i64, !dbg !204
  %17 = getelementptr inbounds i32, ptr %0, i64 %16, !dbg !204
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !205
  %18 = call i1 @pallas.perm(ptr noundef %17, ptr noundef byval(%pallas.fracT) %3), !dbg !206
  %19 = call i1 @pallas.forallSep(i1 %14, i1 %18), !dbg !207
  br label %20

20:                                               ; preds = %9, %5, %2
  %21 = phi i1 [ false, %5 ], [ false, %2 ], [ %19, %9 ], !dbg !194
  ret i1 %21, !dbg !194
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @arrZero(ptr noundef %0, i32 noundef %1) #0 !dbg !208 !pallas.predDef !192 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !209, metadata !DIExpression()), !dbg !210
  call void @llvm.dbg.value(metadata i32 %1, metadata !211, metadata !DIExpression()), !dbg !210
  %4 = icmp ne ptr %0, null, !dbg !212
  br i1 %4, label %5, label %32, !dbg !213

5:                                                ; preds = %2
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !214
  %7 = sext i32 %1 to i64, !dbg !215
  %8 = icmp sge i64 %6, %7, !dbg !216
  br i1 %8, label %9, label %32, !dbg !217

9:                                                ; preds = %5
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !218
  %11 = icmp sle i32 0, %10, !dbg !218
  %12 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !218
  %13 = icmp slt i32 %12, %1, !dbg !218
  %14 = call i1 @pallas.scAnd(i1 %11, i1 %13), !dbg !218
  %15 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !219
  %16 = sext i32 %15 to i64, !dbg !220
  %17 = getelementptr inbounds i32, ptr %0, i64 %16, !dbg !220
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !221
  %18 = call i1 @pallas.perm(ptr noundef %17, ptr noundef byval(%pallas.fracT) %3), !dbg !222
  %19 = call i1 @pallas.forallSep(i1 %14, i1 %18), !dbg !223
  %20 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !224
  %21 = icmp sle i32 0, %20, !dbg !224
  %22 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !224
  %23 = icmp slt i32 %22, %1, !dbg !224
  %24 = call i1 @pallas.scAnd(i1 %21, i1 %23), !dbg !224
  %25 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !225
  %26 = sext i32 %25 to i64, !dbg !226
  %27 = getelementptr inbounds i32, ptr %0, i64 %26, !dbg !226
  %28 = load i32, ptr %27, align 4, !dbg !226
  %29 = icmp eq i32 %28, 0, !dbg !227
  %30 = call i1 @pallas.forall(i1 %24, i1 %29), !dbg !228
  %31 = call i1 @pallas.sepConj(i1 %19, i1 %30), !dbg !229
  br label %32

32:                                               ; preds = %9, %5, %2
  %33 = phi i1 [ false, %5 ], [ false, %2 ], [ %31, %9 ], !dbg !210
  ret i1 %33, !dbg !210
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !230 zeroext i1 @"pallas.unfolding zeroext i1_noundef zeroext i1"(i1 noundef zeroext, i1 noundef zeroext)

declare !pallas.specLib !231 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !232 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !233 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !234 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !235 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !236 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !237 i32 @"pallas.boundVar i32"(ptr)

declare !pallas.specLib !238 i64 @pallas.ptrLength(ptr noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!9, !11, !15, !17}
!llvm.module.flags = !{!18, !19, !20, !21, !22, !23, !24}
!llvm.ident = !{!25, !25}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 23, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "f495d15e4ff3c61d1edad8043ea75677")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 107, type: !3, isLocal: true, isDefinition: true)
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
!32 = !{!33, i1 false, i1 false, !31, !31, !35, !48, !56}
!33 = !{!"pallas.srcLoc", i64 23, i64 1, i64 27, i64 1, !34}
!34 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_pred.c", directory: "", checksumkind: CSK_MD5, checksum: "a7cbc659c62e94ae2a1b91e00a1ab5ca")
!35 = !{!"pallas.requires", !36, ptr @PALLAS_SPEC_0, !31, !31, !37}
!36 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 19, !34}
!37 = !{!38, !45}
!38 = !{!39, !40}
!39 = !DILocalVariable(name: "arr", arg: 1, scope: !26, file: !10, line: 28, type: !29)
!40 = !DILocalVariable(name: "arr", arg: 1, scope: !41, file: !10, line: 24, type: !29)
!41 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !10, file: !10, line: 24, type: !42, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!42 = !DISubroutineType(types: !43)
!43 = !{!44, !29, !30}
!44 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!45 = !{!46, !47}
!46 = !DILocalVariable(name: "size", arg: 2, scope: !26, file: !10, line: 28, type: !30)
!47 = !DILocalVariable(name: "size", arg: 2, scope: !41, file: !10, line: 24, type: !30)
!48 = !{!"pallas.requires", !49, ptr @PALLAS_SPEC_1, !31, !31, !50}
!49 = !{!"pallas.srcLoc", i64 25, i64 1, i64 25, i64 29, !34}
!50 = !{!51, !54}
!51 = !{!39, !52}
!52 = !DILocalVariable(name: "arr", arg: 1, scope: !53, file: !10, line: 25, type: !29)
!53 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !10, file: !10, line: 25, type: !42, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!54 = !{!46, !55}
!55 = !DILocalVariable(name: "size", arg: 2, scope: !53, file: !10, line: 25, type: !30)
!56 = !{!"pallas.ensures", !57, ptr @PALLAS_SPEC_2, !31, !31, !58}
!57 = !{!"pallas.srcLoc", i64 26, i64 1, i64 26, i64 27, !34}
!58 = !{!59, !62}
!59 = !{!39, !60}
!60 = !DILocalVariable(name: "arr", arg: 1, scope: !61, file: !10, line: 26, type: !29)
!61 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !10, file: !10, line: 26, type: !42, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!62 = !{!46, !63}
!63 = !DILocalVariable(name: "size", arg: 2, scope: !61, file: !10, line: 26, type: !30)
!64 = !DILocation(line: 28, column: 20, scope: !26)
!65 = !DILocation(line: 28, column: 29, scope: !26)
!66 = !DILocalVariable(name: "i", scope: !67, file: !10, line: 36, type: !30)
!67 = distinct !DILexicalBlock(scope: !26, file: !10, line: 36, column: 5)
!68 = !DILocation(line: 36, column: 14, scope: !67)
!69 = !DILocation(line: 36, column: 10, scope: !67)
!70 = !DILocation(line: 36, column: 21, scope: !71)
!71 = distinct !DILexicalBlock(scope: !67, file: !10, line: 36, column: 5)
!72 = !DILocation(line: 36, column: 25, scope: !71)
!73 = !DILocation(line: 36, column: 23, scope: !71)
!74 = !DILocation(line: 36, column: 5, scope: !67)
!75 = !DILocation(line: 40, column: 9, scope: !76)
!76 = distinct !DILexicalBlock(scope: !71, file: !10, line: 36, column: 36)
!77 = !{!78, !79}
!78 = !{!"pallas.srcLoc", i64 37, i64 9, i64 39, i64 9, !34}
!79 = !{!"pallas.unfold", !80, ptr @PALLAS_SPEC_6, !31, !31, !81}
!80 = !{!"pallas.srcLoc", i64 38, i64 9, i64 38, i64 35, !34}
!81 = !{!82, !87, !89}
!82 = !{!39, !83}
!83 = !DILocalVariable(name: "arr", arg: 1, scope: !84, file: !10, line: 38, type: !29)
!84 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !10, file: !10, line: 38, type: !85, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!85 = !DISubroutineType(types: !86)
!86 = !{!44, !29, !30, !30}
!87 = !{!46, !88}
!88 = !DILocalVariable(name: "size", arg: 2, scope: !84, file: !10, line: 38, type: !30)
!89 = !{!66, !90}
!90 = !DILocalVariable(name: "i", arg: 3, scope: !84, file: !10, line: 38, type: !30)
!91 = !DILocation(line: 40, column: 13, scope: !76)
!92 = !DILocation(line: 40, column: 16, scope: !76)
!93 = !DILocation(line: 44, column: 5, scope: !76)
!94 = !{!95, !96}
!95 = !{!"pallas.srcLoc", i64 41, i64 9, i64 43, i64 9, !34}
!96 = !{!"pallas.fold", !97, ptr @PALLAS_SPEC_7, !31, !31, !98}
!97 = !{!"pallas.srcLoc", i64 42, i64 9, i64 42, i64 33, !34}
!98 = !{!99, !102, !104}
!99 = !{!39, !100}
!100 = !DILocalVariable(name: "arr", arg: 1, scope: !101, file: !10, line: 42, type: !29)
!101 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !10, file: !10, line: 42, type: !85, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!102 = !{!46, !103}
!103 = !DILocalVariable(name: "size", arg: 2, scope: !101, file: !10, line: 42, type: !30)
!104 = !{!66, !105}
!105 = !DILocalVariable(name: "i", arg: 3, scope: !101, file: !10, line: 42, type: !30)
!106 = !DILocation(line: 36, column: 31, scope: !71)
!107 = !DILocation(line: 36, column: 5, scope: !71)
!108 = distinct !{!108, !74, !109, !110, !111}
!109 = !DILocation(line: 44, column: 5, scope: !67)
!110 = !{!"llvm.loop.mustprogress"}
!111 = !{!"pallas.loopInvBlock", !112, !113, !123, !133}
!112 = !{!"pallas.srcLoc", i64 29, i64 5, i64 35, i64 5, !34}
!113 = !{!"pallas.loopInv", !114, ptr @PALLAS_SPEC_3, !31, !31, !115}
!114 = !{!"pallas.srcLoc", i64 30, i64 5, i64 30, i64 39, !34}
!115 = !{!116, !119, !121}
!116 = !{!39, !117}
!117 = !DILocalVariable(name: "arr", arg: 1, scope: !118, file: !10, line: 30, type: !29)
!118 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !10, file: !10, line: 30, type: !85, scopeLine: 30, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!119 = !{!46, !120}
!120 = !DILocalVariable(name: "size", arg: 2, scope: !118, file: !10, line: 30, type: !30)
!121 = !{!66, !122}
!122 = !DILocalVariable(name: "i", arg: 3, scope: !118, file: !10, line: 30, type: !30)
!123 = !{!"pallas.loopInv", !124, ptr @PALLAS_SPEC_4, !31, !31, !125}
!124 = !{!"pallas.srcLoc", i64 31, i64 5, i64 31, i64 39, !34}
!125 = !{!126, !129, !131}
!126 = !{!39, !127}
!127 = !DILocalVariable(name: "arr", arg: 1, scope: !128, file: !10, line: 31, type: !29)
!128 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !10, file: !10, line: 31, type: !85, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!129 = !{!46, !130}
!130 = !DILocalVariable(name: "size", arg: 2, scope: !128, file: !10, line: 31, type: !30)
!131 = !{!66, !132}
!132 = !DILocalVariable(name: "i", arg: 3, scope: !128, file: !10, line: 31, type: !30)
!133 = !{!"pallas.loopInv", !134, ptr @PALLAS_SPEC_5, !31, !31, !135}
!134 = !{!"pallas.srcLoc", i64 32, i64 5, i64 34, i64 68, !34}
!135 = !{!136, !139, !141}
!136 = !{!39, !137}
!137 = !DILocalVariable(name: "arr", arg: 1, scope: !138, file: !10, line: 32, type: !29)
!138 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !10, file: !10, line: 32, type: !85, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!139 = !{!46, !140}
!140 = !DILocalVariable(name: "size", arg: 2, scope: !138, file: !10, line: 32, type: !30)
!141 = !{!66, !142}
!142 = !DILocalVariable(name: "i", arg: 3, scope: !138, file: !10, line: 32, type: !30)
!143 = !DILocation(line: 50, column: 1, scope: !26)
!144 = !{!145, !146, !154}
!145 = !{!"pallas.srcLoc", i64 46, i64 5, i64 49, i64 5, !34}
!146 = !{!"pallas.unfold", !147, ptr @PALLAS_SPEC_8, !31, !31, !148}
!147 = !{!"pallas.srcLoc", i64 47, i64 5, i64 47, i64 31, !34}
!148 = !{!149, !152}
!149 = !{!39, !150}
!150 = !DILocalVariable(name: "arr", arg: 1, scope: !151, file: !10, line: 47, type: !29)
!151 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !10, file: !10, line: 47, type: !42, scopeLine: 47, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!152 = !{!46, !153}
!153 = !DILocalVariable(name: "size", arg: 2, scope: !151, file: !10, line: 47, type: !30)
!154 = !{!"pallas.fold", !155, ptr @PALLAS_SPEC_9, !31, !31, !156}
!155 = !{!"pallas.srcLoc", i64 48, i64 5, i64 48, i64 28, !34}
!156 = !{!157, !160}
!157 = !{!39, !158}
!158 = !DILocalVariable(name: "arr", arg: 1, scope: !159, file: !10, line: 48, type: !29)
!159 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !10, file: !10, line: 48, type: !42, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!160 = !{!46, !161}
!161 = !DILocalVariable(name: "size", arg: 2, scope: !159, file: !10, line: 48, type: !30)
!162 = !{!""}
!163 = !DILocation(line: 0, scope: !41)
!164 = !DILocation(line: 24, column: 15, scope: !41)
!165 = !DILocation(line: 0, scope: !53)
!166 = !DILocation(line: 25, column: 10, scope: !53)
!167 = !DILocation(line: 0, scope: !61)
!168 = !DILocation(line: 26, column: 9, scope: !61)
!169 = !DILocation(line: 0, scope: !128)
!170 = !DILocation(line: 31, column: 20, scope: !128)
!171 = !DILocation(line: 0, scope: !118)
!172 = !DILocation(line: 30, column: 22, scope: !118)
!173 = !DILocation(line: 30, column: 27, scope: !118)
!174 = !DILocation(line: 30, column: 32, scope: !118)
!175 = !DILocation(line: 0, scope: !138)
!176 = !DILocation(line: 32, column: 37, scope: !138)
!177 = !DILocation(line: 33, column: 33, scope: !138)
!178 = !DILocation(line: 34, column: 49, scope: !138)
!179 = !DILocation(line: 34, column: 45, scope: !138)
!180 = !DILocation(line: 34, column: 62, scope: !138)
!181 = !DILocation(line: 33, column: 25, scope: !138)
!182 = !DILocation(line: 32, column: 20, scope: !138)
!183 = !DILocation(line: 0, scope: !84)
!184 = !DILocation(line: 38, column: 16, scope: !84)
!185 = !DILocation(line: 0, scope: !101)
!186 = !DILocation(line: 42, column: 14, scope: !101)
!187 = !DILocation(line: 0, scope: !151)
!188 = !DILocation(line: 47, column: 12, scope: !151)
!189 = !DILocation(line: 0, scope: !159)
!190 = !DILocation(line: 48, column: 10, scope: !159)
!191 = distinct !DISubprogram(name: "arrWrite", scope: !16, file: !16, line: 11, type: !42, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !15, retainedNodes: !31)
!192 = !{i1 false}
!193 = !DILocalVariable(name: "a", arg: 1, scope: !191, file: !16, line: 11, type: !29)
!194 = !DILocation(line: 0, scope: !191)
!195 = !DILocalVariable(name: "n", arg: 2, scope: !191, file: !16, line: 11, type: !30)
!196 = !DILocation(line: 11, column: 40, scope: !191)
!197 = !DILocation(line: 11, column: 48, scope: !191)
!198 = !DILocation(line: 12, column: 38, scope: !191)
!199 = !DILocation(line: 12, column: 56, scope: !191)
!200 = !DILocation(line: 12, column: 53, scope: !191)
!201 = !DILocation(line: 12, column: 58, scope: !191)
!202 = !DILocation(line: 13, column: 47, scope: !191)
!203 = !DILocation(line: 14, column: 56, scope: !191)
!204 = !DILocation(line: 14, column: 54, scope: !191)
!205 = !DILocation(line: 14, column: 70, scope: !191)
!206 = !DILocation(line: 14, column: 47, scope: !191)
!207 = !DILocation(line: 13, column: 38, scope: !191)
!208 = distinct !DISubprogram(name: "arrZero", scope: !16, file: !16, line: 15, type: !42, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !31)
!209 = !DILocalVariable(name: "a", arg: 1, scope: !208, file: !16, line: 15, type: !29)
!210 = !DILocation(line: 0, scope: !208)
!211 = !DILocalVariable(name: "n", arg: 2, scope: !208, file: !16, line: 15, type: !30)
!212 = !DILocation(line: 15, column: 39, scope: !208)
!213 = !DILocation(line: 15, column: 47, scope: !208)
!214 = !DILocation(line: 16, column: 37, scope: !208)
!215 = !DILocation(line: 16, column: 55, scope: !208)
!216 = !DILocation(line: 16, column: 52, scope: !208)
!217 = !DILocation(line: 16, column: 57, scope: !208)
!218 = !DILocation(line: 17, column: 51, scope: !208)
!219 = !DILocation(line: 18, column: 60, scope: !208)
!220 = !DILocation(line: 18, column: 58, scope: !208)
!221 = !DILocation(line: 18, column: 74, scope: !208)
!222 = !DILocation(line: 18, column: 51, scope: !208)
!223 = !DILocation(line: 17, column: 42, scope: !208)
!224 = !DILocation(line: 19, column: 50, scope: !208)
!225 = !DILocation(line: 20, column: 52, scope: !208)
!226 = !DILocation(line: 20, column: 50, scope: !208)
!227 = !DILocation(line: 20, column: 65, scope: !208)
!228 = !DILocation(line: 19, column: 42, scope: !208)
!229 = !DILocation(line: 17, column: 37, scope: !208)
!230 = !{!"pallas.unfolding"}
!231 = !{!"pallas.sepConj"}
!232 = !{!"pallas.forall"}
!233 = !{!"pallas.forallSep"}
!234 = !{!"pallas.perm"}
!235 = !{!"pallas.fracOf"}
!236 = !{!"pallas.scAnd"}
!237 = !{!"pallas.boundVar"}
!238 = !{!"pallas.ptrLength"}
