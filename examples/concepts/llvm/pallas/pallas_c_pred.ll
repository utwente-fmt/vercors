; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_pred.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [10 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_6, ptr @arrWrite, ptr @arrZero], section "llvm.metadata"
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
  store i32 0, ptr %5, align 4, !dbg !47, !pallas.stmntBlock !48
  br label %6, !dbg !52

6:                                                ; preds = %15, %2
  %7 = load i32, ptr %5, align 4, !dbg !53
  %8 = load i32, ptr %4, align 4, !dbg !55
  %9 = icmp slt i32 %7, %8, !dbg !56
  br i1 %9, label %10, label %18, !dbg !57

10:                                               ; preds = %6
  %11 = load ptr, ptr %3, align 8, !dbg !58
  %12 = load i32, ptr %5, align 4, !dbg !60
  %13 = sext i32 %12 to i64, !dbg !58
  %14 = getelementptr inbounds i32, ptr %11, i64 %13, !dbg !58
  store i32 0, ptr %14, align 4, !dbg !61
  br label %15, !dbg !62

15:                                               ; preds = %10
  %16 = load i32, ptr %5, align 4, !dbg !63
  %17 = add nsw i32 %16, 1, !dbg !63
  store i32 %17, ptr %5, align 4, !dbg !63
  br label %6, !dbg !64, !llvm.loop !65

18:                                               ; preds = %6
  ret void, !dbg !76, !pallas.stmntBlock !77
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !81 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !86, metadata !DIExpression()), !dbg !87
  call void @llvm.dbg.value(metadata i32 %1, metadata !88, metadata !DIExpression()), !dbg !87
  %3 = icmp sge i32 %1, 0, !dbg !89
  ret i1 %3, !dbg !87
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !90 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !91, metadata !DIExpression()), !dbg !92
  call void @llvm.dbg.value(metadata i32 %1, metadata !93, metadata !DIExpression()), !dbg !92
  %3 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !94
  ret i1 %3, !dbg !92
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !95 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !96, metadata !DIExpression()), !dbg !97
  call void @llvm.dbg.value(metadata i32 %1, metadata !98, metadata !DIExpression()), !dbg !97
  %3 = call zeroext i1 @arrZero(ptr noundef %0, i32 noundef %1), !dbg !99
  ret i1 %3, !dbg !97
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !100 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !103, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %1, metadata !105, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %2, metadata !106, metadata !DIExpression()), !dbg !104
  %4 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !107
  %5 = icmp sle i32 0, %4, !dbg !107
  %6 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !107
  %7 = icmp slt i32 %6, %2, !dbg !107
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !107
  %9 = call i32 @pallas.boundVar.0(ptr @.str.1), !dbg !108
  %10 = sext i32 %9 to i64, !dbg !109
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !109
  %12 = load i32, ptr %11, align 4, !dbg !109
  %13 = icmp eq i32 %12, 0, !dbg !110
  %14 = call i1 @pallas.forall(i1 %8, i1 %13), !dbg !111
  ret i1 %14, !dbg !104
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !112 !pallas.exprWrapper !85 {
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !113, metadata !DIExpression()), !dbg !114
  call void @llvm.dbg.value(metadata i32 %1, metadata !115, metadata !DIExpression()), !dbg !114
  call void @llvm.dbg.value(metadata i32 %2, metadata !116, metadata !DIExpression()), !dbg !114
  %5 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !117
  %6 = icmp sle i32 0, %5, !dbg !117
  %7 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !117
  %8 = icmp slt i32 %7, %1, !dbg !117
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !117
  %10 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !118
  %11 = sext i32 %10 to i64, !dbg !119
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !119
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !120
  %13 = call i1 @pallas.perm(ptr noundef %12, ptr noundef byval(%pallas.fracT) %4), !dbg !121
  %14 = call i1 @pallas.forallSep(i1 %9, i1 %13), !dbg !122
  ret i1 %14, !dbg !114
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !123 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !124, metadata !DIExpression()), !dbg !125
  call void @llvm.dbg.value(metadata i32 %1, metadata !126, metadata !DIExpression()), !dbg !125
  call void @llvm.dbg.value(metadata i32 %2, metadata !127, metadata !DIExpression()), !dbg !125
  %4 = icmp sle i32 0, %2, !dbg !128
  br i1 %4, label %5, label %7, !dbg !129

5:                                                ; preds = %3
  %6 = icmp sle i32 %2, %1, !dbg !130
  br label %7

7:                                                ; preds = %5, %3
  %8 = phi i1 [ false, %3 ], [ %6, %5 ], !dbg !125
  ret i1 %8, !dbg !125
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1) #0 !dbg !131 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !132, metadata !DIExpression()), !dbg !133
  call void @llvm.dbg.value(metadata i32 %1, metadata !134, metadata !DIExpression()), !dbg !133
  %3 = call zeroext i1 @arrZero(ptr noundef %0, i32 noundef %1), !dbg !135
  ret i1 %3, !dbg !133
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 !dbg !136 !pallas.exprWrapper !85 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !137, metadata !DIExpression()), !dbg !138
  call void @llvm.dbg.value(metadata i32 %1, metadata !139, metadata !DIExpression()), !dbg !138
  call void @llvm.dbg.value(metadata i32 %2, metadata !140, metadata !DIExpression()), !dbg !138
  %4 = call zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1), !dbg !141
  ret i1 %4, !dbg !138
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @arrWrite(ptr noundef %0, i32 noundef %1) #0 !dbg !142 !pallas.predDef !143 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !144, metadata !DIExpression()), !dbg !145
  call void @llvm.dbg.value(metadata i32 %1, metadata !146, metadata !DIExpression()), !dbg !145
  %4 = icmp ne ptr %0, null, !dbg !147
  br i1 %4, label %5, label %20, !dbg !148

5:                                                ; preds = %2
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !149
  %7 = sext i32 %1 to i64, !dbg !150
  %8 = icmp sge i64 %6, %7, !dbg !151
  br i1 %8, label %9, label %20, !dbg !152

9:                                                ; preds = %5
  %10 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !153
  %11 = icmp sle i32 0, %10, !dbg !153
  %12 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !153
  %13 = icmp slt i32 %12, %1, !dbg !153
  %14 = call i1 @pallas.scAnd(i1 %11, i1 %13), !dbg !153
  %15 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !154
  %16 = sext i32 %15 to i64, !dbg !155
  %17 = getelementptr inbounds i32, ptr %0, i64 %16, !dbg !155
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !156
  %18 = call i1 @pallas.perm(ptr noundef %17, ptr noundef byval(%pallas.fracT) %3), !dbg !157
  %19 = call i1 @pallas.forallSep(i1 %14, i1 %18), !dbg !158
  br label %20

20:                                               ; preds = %9, %5, %2
  %21 = phi i1 [ false, %5 ], [ false, %2 ], [ %19, %9 ], !dbg !145
  ret i1 %21, !dbg !145
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @arrZero(ptr noundef %0, i32 noundef %1) #0 !dbg !159 !pallas.predDef !143 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !160, metadata !DIExpression()), !dbg !161
  call void @llvm.dbg.value(metadata i32 %1, metadata !162, metadata !DIExpression()), !dbg !161
  %4 = icmp ne ptr %0, null, !dbg !163
  br i1 %4, label %5, label %32, !dbg !164

5:                                                ; preds = %2
  %6 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !165
  %7 = sext i32 %1 to i64, !dbg !166
  %8 = icmp sge i64 %6, %7, !dbg !167
  br i1 %8, label %9, label %32, !dbg !168

9:                                                ; preds = %5
  %10 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !169
  %11 = icmp sle i32 0, %10, !dbg !169
  %12 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !169
  %13 = icmp slt i32 %12, %1, !dbg !169
  %14 = call i1 @pallas.scAnd(i1 %11, i1 %13), !dbg !169
  %15 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !170
  %16 = sext i32 %15 to i64, !dbg !171
  %17 = getelementptr inbounds i32, ptr %0, i64 %16, !dbg !171
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !172
  %18 = call i1 @pallas.perm(ptr noundef %17, ptr noundef byval(%pallas.fracT) %3), !dbg !173
  %19 = call i1 @pallas.forallSep(i1 %14, i1 %18), !dbg !174
  %20 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !175
  %21 = icmp sle i32 0, %20, !dbg !175
  %22 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !175
  %23 = icmp slt i32 %22, %1, !dbg !175
  %24 = call i1 @pallas.scAnd(i1 %21, i1 %23), !dbg !175
  %25 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !176
  %26 = sext i32 %25 to i64, !dbg !177
  %27 = getelementptr inbounds i32, ptr %0, i64 %26, !dbg !177
  %28 = load i32, ptr %27, align 4, !dbg !177
  %29 = icmp eq i32 %28, 0, !dbg !178
  %30 = call i1 @pallas.forall(i1 %24, i1 %29), !dbg !179
  %31 = call i1 @pallas.sepConj(i1 %19, i1 %30), !dbg !180
  br label %32

32:                                               ; preds = %9, %5, %2
  %33 = phi i1 [ false, %5 ], [ false, %2 ], [ %31, %9 ], !dbg !161
  ret i1 %33, !dbg !161
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !181 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !182 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !183 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !184 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !185 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !186 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !187 i32 @pallas.boundVar.0(ptr)

declare !pallas.specLib !188 i64 @pallas.ptrLength(ptr noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!9, !11, !15, !17}
!llvm.module.flags = !{!18, !19, !20, !21, !22, !23, !24}
!llvm.ident = !{!25, !25}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 19, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "b98f761a20feb201041c4b511d3aa4c3")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = !DIGlobalVariableExpression(var: !8, expr: !DIExpression())
!8 = distinct !DIGlobalVariable(scope: null, file: !2, line: 83, type: !3, isLocal: true, isDefinition: true)
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !10, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_pred.c", directory: ".", checksumkind: CSK_MD5, checksum: "4ee943daaf9cf6b40aa51605e3031ee7")
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
!26 = distinct !DISubprogram(name: "zero_arr", scope: !10, file: !10, line: 27, type: !27, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!27 = !DISubroutineType(types: !28)
!28 = !{null, !29, !30}
!29 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !30, size: 64)
!30 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!31 = !{}
!32 = !{!33, i1 false, i1 false, !35, !39, !41}
!33 = !{!"pallas.srcLoc", i64 22, i64 1, i64 26, i64 1, !34}
!34 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_pred.c", directory: "", checksumkind: CSK_MD5, checksum: "4ee943daaf9cf6b40aa51605e3031ee7")
!35 = !{!"pallas.requires", !36, ptr @PALLAS_SPEC_0, !37, !38}
!36 = !{!"pallas.srcLoc", i64 23, i64 1, i64 23, i64 19, !34}
!37 = !DILocalVariable(name: "arr", arg: 1, scope: !26, file: !10, line: 27, type: !29)
!38 = !DILocalVariable(name: "size", arg: 2, scope: !26, file: !10, line: 27, type: !30)
!39 = !{!"pallas.requires", !40, ptr @PALLAS_SPEC_1, !37, !38}
!40 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 29, !34}
!41 = !{!"pallas.ensures", !42, ptr @PALLAS_SPEC_2, !37, !38}
!42 = !{!"pallas.srcLoc", i64 25, i64 1, i64 25, i64 27, !34}
!43 = !DILocation(line: 27, column: 20, scope: !26)
!44 = !DILocation(line: 27, column: 29, scope: !26)
!45 = !DILocalVariable(name: "i", scope: !46, file: !10, line: 38, type: !30)
!46 = distinct !DILexicalBlock(scope: !26, file: !10, line: 38, column: 5)
!47 = !DILocation(line: 38, column: 14, scope: !46)
!48 = !{!49, !50}
!49 = !{!"pallas.srcLoc", i64 29, i64 5, i64 29, i64 37, !34}
!50 = !{!"pallas.unfold", !51, ptr @PALLAS_SPEC_6, !37, !38, !45}
!51 = !{!"pallas.srcLoc", i64 29, i64 9, i64 29, i64 35, !34}
!52 = !DILocation(line: 38, column: 10, scope: !46)
!53 = !DILocation(line: 38, column: 21, scope: !54)
!54 = distinct !DILexicalBlock(scope: !46, file: !10, line: 38, column: 5)
!55 = !DILocation(line: 38, column: 25, scope: !54)
!56 = !DILocation(line: 38, column: 23, scope: !54)
!57 = !DILocation(line: 38, column: 5, scope: !46)
!58 = !DILocation(line: 39, column: 9, scope: !59)
!59 = distinct !DILexicalBlock(scope: !54, file: !10, line: 38, column: 36)
!60 = !DILocation(line: 39, column: 13, scope: !59)
!61 = !DILocation(line: 39, column: 16, scope: !59)
!62 = !DILocation(line: 40, column: 5, scope: !59)
!63 = !DILocation(line: 38, column: 31, scope: !54)
!64 = !DILocation(line: 38, column: 5, scope: !54)
!65 = distinct !{!65, !57, !66, !67, !68}
!66 = !DILocation(line: 40, column: 5, scope: !46)
!67 = !{!"llvm.loop.mustprogress"}
!68 = !{!"pallas.loopInv", !69, !70, !72, !74}
!69 = !{!"pallas.srcLoc", i64 31, i64 5, i64 37, i64 5, !34}
!70 = !{!71, ptr @PALLAS_SPEC_3, !37, !38, !45}
!71 = !{!"pallas.srcLoc", i64 32, i64 5, i64 32, i64 39, !34}
!72 = !{!73, ptr @PALLAS_SPEC_4, !37, !38, !45}
!73 = !{!"pallas.srcLoc", i64 33, i64 5, i64 34, i64 62, !34}
!74 = !{!75, ptr @PALLAS_SPEC_5, !37, !38, !45}
!75 = !{!"pallas.srcLoc", i64 35, i64 5, i64 36, i64 50, !34}
!76 = !DILocation(line: 43, column: 1, scope: !26)
!77 = !{!78, !79}
!78 = !{!"pallas.srcLoc", i64 42, i64 5, i64 42, i64 34, !34}
!79 = !{!"pallas.fold", !80, ptr @PALLAS_SPEC_7, !37, !38}
!80 = !{!"pallas.srcLoc", i64 42, i64 9, i64 42, i64 32, !34}
!81 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !10, file: !10, line: 23, type: !82, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!82 = !DISubroutineType(types: !83)
!83 = !{!84, !29, !30}
!84 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!85 = !{!""}
!86 = !DILocalVariable(name: "arr", arg: 1, scope: !81, file: !10, line: 23, type: !29)
!87 = !DILocation(line: 0, scope: !81)
!88 = !DILocalVariable(name: "size", arg: 2, scope: !81, file: !10, line: 23, type: !30)
!89 = !DILocation(line: 23, column: 15, scope: !81)
!90 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !10, file: !10, line: 24, type: !82, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!91 = !DILocalVariable(name: "arr", arg: 1, scope: !90, file: !10, line: 24, type: !29)
!92 = !DILocation(line: 0, scope: !90)
!93 = !DILocalVariable(name: "size", arg: 2, scope: !90, file: !10, line: 24, type: !30)
!94 = !DILocation(line: 24, column: 10, scope: !90)
!95 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !10, file: !10, line: 25, type: !82, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!96 = !DILocalVariable(name: "arr", arg: 1, scope: !95, file: !10, line: 25, type: !29)
!97 = !DILocation(line: 0, scope: !95)
!98 = !DILocalVariable(name: "size", arg: 2, scope: !95, file: !10, line: 25, type: !30)
!99 = !DILocation(line: 25, column: 9, scope: !95)
!100 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !10, file: !10, line: 35, type: !101, scopeLine: 35, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!101 = !DISubroutineType(types: !102)
!102 = !{!84, !29, !30, !30}
!103 = !DILocalVariable(name: "arr", arg: 1, scope: !100, file: !10, line: 35, type: !29)
!104 = !DILocation(line: 0, scope: !100)
!105 = !DILocalVariable(name: "size", arg: 2, scope: !100, file: !10, line: 35, type: !30)
!106 = !DILocalVariable(name: "i", arg: 3, scope: !100, file: !10, line: 35, type: !30)
!107 = !DILocation(line: 35, column: 28, scope: !100)
!108 = !DILocation(line: 36, column: 32, scope: !100)
!109 = !DILocation(line: 36, column: 28, scope: !100)
!110 = !DILocation(line: 36, column: 45, scope: !100)
!111 = !DILocation(line: 35, column: 20, scope: !100)
!112 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !10, file: !10, line: 33, type: !101, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!113 = !DILocalVariable(name: "arr", arg: 1, scope: !112, file: !10, line: 33, type: !29)
!114 = !DILocation(line: 0, scope: !112)
!115 = !DILocalVariable(name: "size", arg: 2, scope: !112, file: !10, line: 33, type: !30)
!116 = !DILocalVariable(name: "i", arg: 3, scope: !112, file: !10, line: 33, type: !30)
!117 = !DILocation(line: 33, column: 29, scope: !112)
!118 = !DILocation(line: 34, column: 40, scope: !112)
!119 = !DILocation(line: 34, column: 36, scope: !112)
!120 = !DILocation(line: 34, column: 54, scope: !112)
!121 = !DILocation(line: 34, column: 29, scope: !112)
!122 = !DILocation(line: 33, column: 20, scope: !112)
!123 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !10, file: !10, line: 32, type: !101, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!124 = !DILocalVariable(name: "arr", arg: 1, scope: !123, file: !10, line: 32, type: !29)
!125 = !DILocation(line: 0, scope: !123)
!126 = !DILocalVariable(name: "size", arg: 2, scope: !123, file: !10, line: 32, type: !30)
!127 = !DILocalVariable(name: "i", arg: 3, scope: !123, file: !10, line: 32, type: !30)
!128 = !DILocation(line: 32, column: 22, scope: !123)
!129 = !DILocation(line: 32, column: 27, scope: !123)
!130 = !DILocation(line: 32, column: 32, scope: !123)
!131 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !10, file: !10, line: 42, type: !82, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!132 = !DILocalVariable(name: "arr", arg: 1, scope: !131, file: !10, line: 42, type: !29)
!133 = !DILocation(line: 0, scope: !131)
!134 = !DILocalVariable(name: "size", arg: 2, scope: !131, file: !10, line: 42, type: !30)
!135 = !DILocation(line: 42, column: 14, scope: !131)
!136 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !10, file: !10, line: 29, type: !101, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !9, retainedNodes: !31)
!137 = !DILocalVariable(name: "arr", arg: 1, scope: !136, file: !10, line: 29, type: !29)
!138 = !DILocation(line: 0, scope: !136)
!139 = !DILocalVariable(name: "size", arg: 2, scope: !136, file: !10, line: 29, type: !30)
!140 = !DILocalVariable(name: "i", arg: 3, scope: !136, file: !10, line: 29, type: !30)
!141 = !DILocation(line: 29, column: 16, scope: !136)
!142 = distinct !DISubprogram(name: "arrWrite", scope: !16, file: !16, line: 10, type: !82, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !15, retainedNodes: !31)
!143 = !{i1 false}
!144 = !DILocalVariable(name: "a", arg: 1, scope: !142, file: !16, line: 10, type: !29)
!145 = !DILocation(line: 0, scope: !142)
!146 = !DILocalVariable(name: "n", arg: 2, scope: !142, file: !16, line: 10, type: !30)
!147 = !DILocation(line: 10, column: 40, scope: !142)
!148 = !DILocation(line: 10, column: 48, scope: !142)
!149 = !DILocation(line: 11, column: 38, scope: !142)
!150 = !DILocation(line: 11, column: 56, scope: !142)
!151 = !DILocation(line: 11, column: 53, scope: !142)
!152 = !DILocation(line: 11, column: 58, scope: !142)
!153 = !DILocation(line: 12, column: 47, scope: !142)
!154 = !DILocation(line: 13, column: 56, scope: !142)
!155 = !DILocation(line: 13, column: 54, scope: !142)
!156 = !DILocation(line: 13, column: 70, scope: !142)
!157 = !DILocation(line: 13, column: 47, scope: !142)
!158 = !DILocation(line: 12, column: 38, scope: !142)
!159 = distinct !DISubprogram(name: "arrZero", scope: !16, file: !16, line: 14, type: !82, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !17, retainedNodes: !31)
!160 = !DILocalVariable(name: "a", arg: 1, scope: !159, file: !16, line: 14, type: !29)
!161 = !DILocation(line: 0, scope: !159)
!162 = !DILocalVariable(name: "n", arg: 2, scope: !159, file: !16, line: 14, type: !30)
!163 = !DILocation(line: 14, column: 39, scope: !159)
!164 = !DILocation(line: 14, column: 47, scope: !159)
!165 = !DILocation(line: 15, column: 37, scope: !159)
!166 = !DILocation(line: 15, column: 55, scope: !159)
!167 = !DILocation(line: 15, column: 52, scope: !159)
!168 = !DILocation(line: 15, column: 57, scope: !159)
!169 = !DILocation(line: 16, column: 51, scope: !159)
!170 = !DILocation(line: 17, column: 60, scope: !159)
!171 = !DILocation(line: 17, column: 58, scope: !159)
!172 = !DILocation(line: 17, column: 74, scope: !159)
!173 = !DILocation(line: 17, column: 51, scope: !159)
!174 = !DILocation(line: 16, column: 42, scope: !159)
!175 = !DILocation(line: 18, column: 50, scope: !159)
!176 = !DILocation(line: 19, column: 52, scope: !159)
!177 = !DILocation(line: 19, column: 50, scope: !159)
!178 = !DILocation(line: 19, column: 65, scope: !159)
!179 = !DILocation(line: 18, column: 42, scope: !159)
!180 = !DILocation(line: 16, column: 37, scope: !159)
!181 = !{!"pallas.sepConj"}
!182 = !{!"pallas.forall"}
!183 = !{!"pallas.forallSep"}
!184 = !{!"pallas.perm"}
!185 = !{!"pallas.fracOf"}
!186 = !{!"pallas.scAnd"}
!187 = !{!"pallas.boundVar"}
!188 = !{!"pallas.ptrLength"}
