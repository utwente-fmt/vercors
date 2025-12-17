; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_lower_bound.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [11 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_6], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(ptr noundef %0, i32 noundef %1) #0 !dbg !21 !pallas.fcontract !27 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !32, metadata !DIExpression()), !dbg !44
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !33, metadata !DIExpression()), !dbg !45
  call void @llvm.dbg.declare(metadata ptr %5, metadata !46, metadata !DIExpression()), !dbg !47
  store i32 1, ptr %5, align 4, !dbg !47
  call void @llvm.dbg.declare(metadata ptr %6, metadata !48, metadata !DIExpression()), !dbg !49
  %7 = load ptr, ptr %3, align 8, !dbg !50
  %8 = getelementptr inbounds i32, ptr %7, i64 0, !dbg !50
  %9 = load i32, ptr %8, align 4, !dbg !50
  store i32 %9, ptr %6, align 4, !dbg !49
  br label %10, !dbg !51

10:                                               ; preds = %28, %2
  %11 = load i32, ptr %5, align 4, !dbg !52
  %12 = load i32, ptr %4, align 4, !dbg !53
  %13 = icmp slt i32 %11, %12, !dbg !54
  br i1 %13, label %14, label %31, !dbg !51

14:                                               ; preds = %10
  %15 = load ptr, ptr %3, align 8, !dbg !55
  %16 = load i32, ptr %5, align 4, !dbg !58
  %17 = sext i32 %16 to i64, !dbg !55
  %18 = getelementptr inbounds i32, ptr %15, i64 %17, !dbg !55
  %19 = load i32, ptr %18, align 4, !dbg !55
  %20 = load i32, ptr %6, align 4, !dbg !59
  %21 = icmp slt i32 %19, %20, !dbg !60
  br i1 %21, label %22, label %28, !dbg !61

22:                                               ; preds = %14
  %23 = load ptr, ptr %3, align 8, !dbg !62
  %24 = load i32, ptr %5, align 4, !dbg !63
  %25 = sext i32 %24 to i64, !dbg !62
  %26 = getelementptr inbounds i32, ptr %23, i64 %25, !dbg !62
  %27 = load i32, ptr %26, align 4, !dbg !62
  store i32 %27, ptr %6, align 4, !dbg !64
  br label %28, !dbg !65

28:                                               ; preds = %22, %14
  %29 = load i32, ptr %5, align 4, !dbg !66
  %30 = add nsw i32 %29, 1, !dbg !66
  store i32 %30, ptr %5, align 4, !dbg !66
  br label %10, !dbg !51, !llvm.loop !67

31:                                               ; preds = %10
  %32 = load i32, ptr %6, align 4, !dbg !82
  ret i32 %32, !dbg !83
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !84 !pallas.exprWrapper !88 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !89, metadata !DIExpression()), !dbg !90
  call void @llvm.dbg.value(metadata i32 %1, metadata !91, metadata !DIExpression()), !dbg !90
  %3 = icmp ne ptr %0, null, !dbg !92
  br i1 %3, label %4, label %6, !dbg !93

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !94
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !90
  ret i1 %7, !dbg !90
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !95 !pallas.exprWrapper !88 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !96, metadata !DIExpression()), !dbg !97
  call void @llvm.dbg.value(metadata i32 %1, metadata !98, metadata !DIExpression()), !dbg !97
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !99
  %4 = sext i32 %1 to i64, !dbg !100
  %5 = icmp eq i64 %3, %4, !dbg !101
  ret i1 %5, !dbg !97
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !102 !pallas.exprWrapper !88 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !103, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %1, metadata !105, metadata !DIExpression()), !dbg !104
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !106
  %5 = icmp sle i32 0, %4, !dbg !107
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !108
  %7 = icmp slt i32 %6, %1, !dbg !109
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !110
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !111
  %10 = sext i32 %9 to i64, !dbg !112
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !112
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !113
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !114
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !115
  ret i1 %13, !dbg !104
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1) #0 !dbg !116 !pallas.exprWrapper !88 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !117, metadata !DIExpression()), !dbg !118
  call void @llvm.dbg.value(metadata i32 %1, metadata !119, metadata !DIExpression()), !dbg !118
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !120
  %4 = sext i32 %1 to i64, !dbg !121
  %5 = icmp eq i64 %3, %4, !dbg !122
  ret i1 %5, !dbg !118
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1) #0 !dbg !123 !pallas.exprWrapper !88 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !124, metadata !DIExpression()), !dbg !125
  call void @llvm.dbg.value(metadata i32 %1, metadata !126, metadata !DIExpression()), !dbg !125
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !127
  %5 = icmp sle i32 0, %4, !dbg !128
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !129
  %7 = icmp slt i32 %6, %1, !dbg !130
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !131
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !132
  %10 = sext i32 %9 to i64, !dbg !133
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !133
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !134
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !135
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !136
  ret i1 %13, !dbg !125
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1) #0 !dbg !137 !pallas.exprWrapper !88 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !138, metadata !DIExpression()), !dbg !139
  call void @llvm.dbg.value(metadata i32 %1, metadata !140, metadata !DIExpression()), !dbg !139
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !141
  %4 = icmp sle i32 0, %3, !dbg !142
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !143
  %6 = icmp slt i32 %5, %1, !dbg !144
  %7 = call i1 @pallas.scAnd(i1 %4, i1 %6), !dbg !145
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !146
  %9 = sext i32 %8 to i64, !dbg !147
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !147
  %11 = load i32, ptr %10, align 4, !dbg !147
  %12 = call i32 @"pallas.result i32"(), !dbg !148
  %13 = icmp sge i32 %11, %12, !dbg !149
  %14 = call i1 @pallas.forall(i1 %7, i1 %13), !dbg !150
  ret i1 %14, !dbg !139
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !151 !pallas.exprWrapper !88 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !154, metadata !DIExpression()), !dbg !155
  call void @llvm.dbg.value(metadata i32 %1, metadata !156, metadata !DIExpression()), !dbg !155
  call void @llvm.dbg.value(metadata i32 %2, metadata !157, metadata !DIExpression()), !dbg !155
  call void @llvm.dbg.value(metadata i32 %3, metadata !158, metadata !DIExpression()), !dbg !155
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !159
  %7 = icmp sle i32 0, %6, !dbg !160
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !161
  %9 = icmp slt i32 %8, %1, !dbg !162
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !163
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !164
  %12 = sext i32 %11 to i64, !dbg !165
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !165
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !166
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %5), !dbg !167
  %15 = call i1 @pallas.forallSep(i1 %10, i1 %14), !dbg !168
  ret i1 %15, !dbg !155
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !169 !pallas.exprWrapper !88 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !170, metadata !DIExpression()), !dbg !171
  call void @llvm.dbg.value(metadata i32 %1, metadata !172, metadata !DIExpression()), !dbg !171
  call void @llvm.dbg.value(metadata i32 %2, metadata !173, metadata !DIExpression()), !dbg !171
  call void @llvm.dbg.value(metadata i32 %3, metadata !174, metadata !DIExpression()), !dbg !171
  %5 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !175
  %6 = sext i32 %1 to i64, !dbg !176
  %7 = icmp eq i64 %5, %6, !dbg !177
  ret i1 %7, !dbg !171
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !178 !pallas.exprWrapper !88 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !179, metadata !DIExpression()), !dbg !180
  call void @llvm.dbg.value(metadata i32 %1, metadata !181, metadata !DIExpression()), !dbg !180
  call void @llvm.dbg.value(metadata i32 %2, metadata !182, metadata !DIExpression()), !dbg !180
  call void @llvm.dbg.value(metadata i32 %3, metadata !183, metadata !DIExpression()), !dbg !180
  %5 = icmp sle i32 1, %2, !dbg !184
  %6 = icmp sle i32 %2, %1, !dbg !185
  %7 = call i1 @pallas.scAnd(i1 %5, i1 %6), !dbg !186
  ret i1 %7, !dbg !180
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !187 !pallas.exprWrapper !88 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !188, metadata !DIExpression()), !dbg !189
  call void @llvm.dbg.value(metadata i32 %1, metadata !190, metadata !DIExpression()), !dbg !189
  call void @llvm.dbg.value(metadata i32 %2, metadata !191, metadata !DIExpression()), !dbg !189
  call void @llvm.dbg.value(metadata i32 %3, metadata !192, metadata !DIExpression()), !dbg !189
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !193
  %6 = icmp sle i32 0, %5, !dbg !194
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !195
  %8 = icmp slt i32 %7, %2, !dbg !196
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !197
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !198
  %11 = sext i32 %10 to i64, !dbg !199
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !199
  %13 = load i32, ptr %12, align 4, !dbg !199
  %14 = icmp sge i32 %13, %3, !dbg !200
  %15 = call i1 @pallas.forall(i1 %9, i1 %14), !dbg !201
  ret i1 %15, !dbg !189
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !202 !pallas.exprWrapper !88 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !203, metadata !DIExpression()), !dbg !204
  call void @llvm.dbg.value(metadata i32 %1, metadata !205, metadata !DIExpression()), !dbg !204
  call void @llvm.dbg.value(metadata i32 %2, metadata !206, metadata !DIExpression()), !dbg !204
  call void @llvm.dbg.value(metadata i32 %3, metadata !207, metadata !DIExpression()), !dbg !204
  %5 = icmp ne ptr %0, null, !dbg !208
  ret i1 %5, !dbg !204
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !209 i32 @"pallas.result i32"()

declare !pallas.specLib !210 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !211 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !212 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !213 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !214 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !215 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !216 i32 @"pallas.boundVar i32"(ptr)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!7, !9}
!llvm.module.flags = !{!13, !14, !15, !16, !17, !18, !19}
!llvm.ident = !{!20, !20}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 36, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "81e21ec0b7c1f7a315159891d8901370")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = distinct !DICompileUnit(language: DW_LANG_C11, file: !8, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!8 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_lower_bound.c", directory: ".", checksumkind: CSK_MD5, checksum: "efcb7277c13ae93349fe27ab9ec2fd39")
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
!21 = distinct !DISubprogram(name: "foo", scope: !8, file: !8, line: 19, type: !22, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!22 = !DISubroutineType(types: !23)
!23 = !{!24, !25, !24}
!24 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!25 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !24, size: 64)
!26 = !{}
!27 = !{!28, i1 false, i1 false, !30, !34, !36, !38, !40, !42}
!28 = !{!"pallas.srcLoc", i64 8, i64 1, i64 18, i64 1, !29}
!29 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_lower_bound.c", directory: "", checksumkind: CSK_MD5, checksum: "efcb7277c13ae93349fe27ab9ec2fd39")
!30 = !{!"pallas.requires", !31, ptr @PALLAS_SPEC_0, !32, !33}
!31 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 30, !29}
!32 = !DILocalVariable(name: "arr", arg: 1, scope: !21, file: !8, line: 19, type: !25)
!33 = !DILocalVariable(name: "n", arg: 2, scope: !21, file: !8, line: 19, type: !24)
!34 = !{!"pallas.requires", !35, ptr @PALLAS_SPEC_1, !32, !33}
!35 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 31, !29}
!36 = !{!"pallas.requires", !37, ptr @PALLAS_SPEC_2, !32, !33}
!37 = !{!"pallas.srcLoc", i64 11, i64 1, i64 12, i64 87, !29}
!38 = !{!"pallas.ensures", !39, ptr @PALLAS_SPEC_3, !32, !33}
!39 = !{!"pallas.srcLoc", i64 13, i64 1, i64 13, i64 30, !29}
!40 = !{!"pallas.ensures", !41, ptr @PALLAS_SPEC_4, !32, !33}
!41 = !{!"pallas.srcLoc", i64 14, i64 1, i64 15, i64 86, !29}
!42 = !{!"pallas.ensures", !43, ptr @PALLAS_SPEC_5, !32, !33}
!43 = !{!"pallas.srcLoc", i64 16, i64 1, i64 17, i64 78, !29}
!44 = !DILocation(line: 19, column: 14, scope: !21)
!45 = !DILocation(line: 19, column: 23, scope: !21)
!46 = !DILocalVariable(name: "idx", scope: !21, file: !8, line: 20, type: !24)
!47 = !DILocation(line: 20, column: 9, scope: !21)
!48 = !DILocalVariable(name: "min", scope: !21, file: !8, line: 21, type: !24)
!49 = !DILocation(line: 21, column: 9, scope: !21)
!50 = !DILocation(line: 21, column: 15, scope: !21)
!51 = !DILocation(line: 32, column: 5, scope: !21)
!52 = !DILocation(line: 32, column: 12, scope: !21)
!53 = !DILocation(line: 32, column: 18, scope: !21)
!54 = !DILocation(line: 32, column: 16, scope: !21)
!55 = !DILocation(line: 33, column: 13, scope: !56)
!56 = distinct !DILexicalBlock(scope: !57, file: !8, line: 33, column: 13)
!57 = distinct !DILexicalBlock(scope: !21, file: !8, line: 32, column: 21)
!58 = !DILocation(line: 33, column: 17, scope: !56)
!59 = !DILocation(line: 33, column: 24, scope: !56)
!60 = !DILocation(line: 33, column: 22, scope: !56)
!61 = !DILocation(line: 33, column: 13, scope: !57)
!62 = !DILocation(line: 34, column: 19, scope: !56)
!63 = !DILocation(line: 34, column: 23, scope: !56)
!64 = !DILocation(line: 34, column: 17, scope: !56)
!65 = !DILocation(line: 34, column: 13, scope: !56)
!66 = !DILocation(line: 35, column: 12, scope: !57)
!67 = distinct !{!67, !51, !68, !69, !70}
!68 = !DILocation(line: 36, column: 5, scope: !21)
!69 = !{!"llvm.loop.mustprogress"}
!70 = !{!"pallas.loopInv", !71, !72, !74, !76, !78, !80}
!71 = !{!"pallas.srcLoc", i64 23, i64 5, i64 31, i64 5, !29}
!72 = !{!73, ptr @PALLAS_SPEC_6, !32, !33, !46, !48}
!73 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 31, !29}
!74 = !{!75, ptr @PALLAS_SPEC_7, !32, !33, !46, !48}
!75 = !{!"pallas.srcLoc", i64 25, i64 5, i64 25, i64 44, !29}
!76 = !{!77, ptr @PALLAS_SPEC_8, !32, !33, !46, !48}
!77 = !{!"pallas.srcLoc", i64 26, i64 5, i64 26, i64 41, !29}
!78 = !{!79, ptr @PALLAS_SPEC_9, !32, !33, !46, !48}
!79 = !{!"pallas.srcLoc", i64 27, i64 5, i64 28, i64 97, !29}
!80 = !{!81, ptr @PALLAS_SPEC_10, !32, !33, !46, !48}
!81 = !{!"pallas.srcLoc", i64 29, i64 5, i64 30, i64 82, !29}
!82 = !DILocation(line: 38, column: 12, scope: !21)
!83 = !DILocation(line: 38, column: 5, scope: !21)
!84 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !8, file: !8, line: 9, type: !85, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!85 = !DISubroutineType(types: !86)
!86 = !{!87, !25, !24}
!87 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!88 = !{!""}
!89 = !DILocalVariable(name: "arr", arg: 1, scope: !84, file: !8, line: 9, type: !25)
!90 = !DILocation(line: 0, scope: !84)
!91 = !DILocalVariable(name: "n", arg: 2, scope: !84, file: !8, line: 9, type: !24)
!92 = !DILocation(line: 9, column: 14, scope: !84)
!93 = !DILocation(line: 9, column: 22, scope: !84)
!94 = !DILocation(line: 9, column: 27, scope: !84)
!95 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !8, file: !8, line: 10, type: !85, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!96 = !DILocalVariable(name: "arr", arg: 1, scope: !95, file: !8, line: 10, type: !25)
!97 = !DILocation(line: 0, scope: !95)
!98 = !DILocalVariable(name: "n", arg: 2, scope: !95, file: !8, line: 10, type: !24)
!99 = !DILocation(line: 10, column: 10, scope: !95)
!100 = !DILocation(line: 10, column: 30, scope: !95)
!101 = !DILocation(line: 10, column: 27, scope: !95)
!102 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !8, file: !8, line: 11, type: !85, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!103 = !DILocalVariable(name: "arr", arg: 1, scope: !102, file: !8, line: 11, type: !25)
!104 = !DILocation(line: 0, scope: !102)
!105 = !DILocalVariable(name: "n", arg: 2, scope: !102, file: !8, line: 11, type: !24)
!106 = !DILocation(line: 11, column: 29, scope: !102)
!107 = !DILocation(line: 11, column: 26, scope: !102)
!108 = !DILocation(line: 12, column: 29, scope: !102)
!109 = !DILocation(line: 12, column: 41, scope: !102)
!110 = !DILocation(line: 11, column: 19, scope: !102)
!111 = !DILocation(line: 12, column: 58, scope: !102)
!112 = !DILocation(line: 12, column: 54, scope: !102)
!113 = !DILocation(line: 12, column: 72, scope: !102)
!114 = !DILocation(line: 12, column: 47, scope: !102)
!115 = !DILocation(line: 11, column: 10, scope: !102)
!116 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !8, file: !8, line: 13, type: !85, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!117 = !DILocalVariable(name: "arr", arg: 1, scope: !116, file: !8, line: 13, type: !25)
!118 = !DILocation(line: 0, scope: !116)
!119 = !DILocalVariable(name: "n", arg: 2, scope: !116, file: !8, line: 13, type: !24)
!120 = !DILocation(line: 13, column: 9, scope: !116)
!121 = !DILocation(line: 13, column: 29, scope: !116)
!122 = !DILocation(line: 13, column: 26, scope: !116)
!123 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !8, file: !8, line: 14, type: !85, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!124 = !DILocalVariable(name: "arr", arg: 1, scope: !123, file: !8, line: 14, type: !25)
!125 = !DILocation(line: 0, scope: !123)
!126 = !DILocalVariable(name: "n", arg: 2, scope: !123, file: !8, line: 14, type: !24)
!127 = !DILocation(line: 14, column: 28, scope: !123)
!128 = !DILocation(line: 14, column: 25, scope: !123)
!129 = !DILocation(line: 15, column: 28, scope: !123)
!130 = !DILocation(line: 15, column: 40, scope: !123)
!131 = !DILocation(line: 14, column: 18, scope: !123)
!132 = !DILocation(line: 15, column: 57, scope: !123)
!133 = !DILocation(line: 15, column: 53, scope: !123)
!134 = !DILocation(line: 15, column: 71, scope: !123)
!135 = !DILocation(line: 15, column: 46, scope: !123)
!136 = !DILocation(line: 14, column: 9, scope: !123)
!137 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !8, file: !8, line: 16, type: !85, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!138 = !DILocalVariable(name: "arr", arg: 1, scope: !137, file: !8, line: 16, type: !25)
!139 = !DILocation(line: 0, scope: !137)
!140 = !DILocalVariable(name: "n", arg: 2, scope: !137, file: !8, line: 16, type: !24)
!141 = !DILocation(line: 16, column: 27, scope: !137)
!142 = !DILocation(line: 16, column: 24, scope: !137)
!143 = !DILocation(line: 17, column: 27, scope: !137)
!144 = !DILocation(line: 17, column: 39, scope: !137)
!145 = !DILocation(line: 16, column: 17, scope: !137)
!146 = !DILocation(line: 17, column: 49, scope: !137)
!147 = !DILocation(line: 17, column: 45, scope: !137)
!148 = !DILocation(line: 17, column: 65, scope: !137)
!149 = !DILocation(line: 17, column: 62, scope: !137)
!150 = !DILocation(line: 16, column: 9, scope: !137)
!151 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !8, file: !8, line: 27, type: !152, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!152 = !DISubroutineType(types: !153)
!153 = !{!87, !25, !24, !24, !24}
!154 = !DILocalVariable(name: "arr", arg: 1, scope: !151, file: !8, line: 27, type: !25)
!155 = !DILocation(line: 0, scope: !151)
!156 = !DILocalVariable(name: "n", arg: 2, scope: !151, file: !8, line: 27, type: !24)
!157 = !DILocalVariable(name: "idx", arg: 3, scope: !151, file: !8, line: 27, type: !24)
!158 = !DILocalVariable(name: "min", arg: 4, scope: !151, file: !8, line: 27, type: !24)
!159 = !DILocation(line: 27, column: 39, scope: !151)
!160 = !DILocation(line: 27, column: 36, scope: !151)
!161 = !DILocation(line: 28, column: 39, scope: !151)
!162 = !DILocation(line: 28, column: 51, scope: !151)
!163 = !DILocation(line: 27, column: 29, scope: !151)
!164 = !DILocation(line: 28, column: 68, scope: !151)
!165 = !DILocation(line: 28, column: 64, scope: !151)
!166 = !DILocation(line: 28, column: 82, scope: !151)
!167 = !DILocation(line: 28, column: 57, scope: !151)
!168 = !DILocation(line: 27, column: 20, scope: !151)
!169 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !8, file: !8, line: 26, type: !152, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!170 = !DILocalVariable(name: "arr", arg: 1, scope: !169, file: !8, line: 26, type: !25)
!171 = !DILocation(line: 0, scope: !169)
!172 = !DILocalVariable(name: "n", arg: 2, scope: !169, file: !8, line: 26, type: !24)
!173 = !DILocalVariable(name: "idx", arg: 3, scope: !169, file: !8, line: 26, type: !24)
!174 = !DILocalVariable(name: "min", arg: 4, scope: !169, file: !8, line: 26, type: !24)
!175 = !DILocation(line: 26, column: 20, scope: !169)
!176 = !DILocation(line: 26, column: 40, scope: !169)
!177 = !DILocation(line: 26, column: 37, scope: !169)
!178 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !8, file: !8, line: 25, type: !152, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!179 = !DILocalVariable(name: "arr", arg: 1, scope: !178, file: !8, line: 25, type: !25)
!180 = !DILocation(line: 0, scope: !178)
!181 = !DILocalVariable(name: "n", arg: 2, scope: !178, file: !8, line: 25, type: !24)
!182 = !DILocalVariable(name: "idx", arg: 3, scope: !178, file: !8, line: 25, type: !24)
!183 = !DILocalVariable(name: "min", arg: 4, scope: !178, file: !8, line: 25, type: !24)
!184 = !DILocation(line: 25, column: 27, scope: !178)
!185 = !DILocation(line: 25, column: 39, scope: !178)
!186 = !DILocation(line: 25, column: 20, scope: !178)
!187 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !8, file: !8, line: 29, type: !152, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!188 = !DILocalVariable(name: "arr", arg: 1, scope: !187, file: !8, line: 29, type: !25)
!189 = !DILocation(line: 0, scope: !187)
!190 = !DILocalVariable(name: "n", arg: 2, scope: !187, file: !8, line: 29, type: !24)
!191 = !DILocalVariable(name: "idx", arg: 3, scope: !187, file: !8, line: 29, type: !24)
!192 = !DILocalVariable(name: "min", arg: 4, scope: !187, file: !8, line: 29, type: !24)
!193 = !DILocation(line: 29, column: 38, scope: !187)
!194 = !DILocation(line: 29, column: 35, scope: !187)
!195 = !DILocation(line: 30, column: 38, scope: !187)
!196 = !DILocation(line: 30, column: 50, scope: !187)
!197 = !DILocation(line: 29, column: 28, scope: !187)
!198 = !DILocation(line: 30, column: 62, scope: !187)
!199 = !DILocation(line: 30, column: 58, scope: !187)
!200 = !DILocation(line: 30, column: 75, scope: !187)
!201 = !DILocation(line: 29, column: 20, scope: !187)
!202 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !8, file: !8, line: 24, type: !152, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!203 = !DILocalVariable(name: "arr", arg: 1, scope: !202, file: !8, line: 24, type: !25)
!204 = !DILocation(line: 0, scope: !202)
!205 = !DILocalVariable(name: "n", arg: 2, scope: !202, file: !8, line: 24, type: !24)
!206 = !DILocalVariable(name: "idx", arg: 3, scope: !202, file: !8, line: 24, type: !24)
!207 = !DILocalVariable(name: "min", arg: 4, scope: !202, file: !8, line: 24, type: !24)
!208 = !DILocation(line: 24, column: 24, scope: !202)
!209 = !{!"pallas.result"}
!210 = !{!"pallas.forallSep"}
!211 = !{!"pallas.perm"}
!212 = !{!"pallas.fracOf"}
!213 = !{!"pallas.ptrLength"}
!214 = !{!"pallas.forall"}
!215 = !{!"pallas.scAnd"}
!216 = !{!"pallas.boundVar"}
