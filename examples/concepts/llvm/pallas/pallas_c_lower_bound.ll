; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_lower_bound.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [11 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_8], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(ptr noundef %0, i32 noundef %1) #0 !dbg !21 !pallas.fcontract !27 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !34, metadata !DIExpression()), !dbg !83
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !41, metadata !DIExpression()), !dbg !84
  call void @llvm.dbg.declare(metadata ptr %5, metadata !85, metadata !DIExpression()), !dbg !86
  store i32 1, ptr %5, align 4, !dbg !86
  call void @llvm.dbg.declare(metadata ptr %6, metadata !87, metadata !DIExpression()), !dbg !88
  %7 = load ptr, ptr %3, align 8, !dbg !89
  %8 = getelementptr inbounds i32, ptr %7, i64 0, !dbg !89
  %9 = load i32, ptr %8, align 4, !dbg !89
  store i32 %9, ptr %6, align 4, !dbg !88
  br label %10, !dbg !90

10:                                               ; preds = %28, %2
  %11 = load i32, ptr %5, align 4, !dbg !91
  %12 = load i32, ptr %4, align 4, !dbg !92
  %13 = icmp slt i32 %11, %12, !dbg !93
  br i1 %13, label %14, label %31, !dbg !90

14:                                               ; preds = %10
  %15 = load ptr, ptr %3, align 8, !dbg !94
  %16 = load i32, ptr %5, align 4, !dbg !97
  %17 = sext i32 %16 to i64, !dbg !94
  %18 = getelementptr inbounds i32, ptr %15, i64 %17, !dbg !94
  %19 = load i32, ptr %18, align 4, !dbg !94
  %20 = load i32, ptr %6, align 4, !dbg !98
  %21 = icmp slt i32 %19, %20, !dbg !99
  br i1 %21, label %22, label %28, !dbg !100

22:                                               ; preds = %14
  %23 = load ptr, ptr %3, align 8, !dbg !101
  %24 = load i32, ptr %5, align 4, !dbg !102
  %25 = sext i32 %24 to i64, !dbg !101
  %26 = getelementptr inbounds i32, ptr %23, i64 %25, !dbg !101
  %27 = load i32, ptr %26, align 4, !dbg !101
  store i32 %27, ptr %6, align 4, !dbg !103
  br label %28, !dbg !104

28:                                               ; preds = %22, %14
  %29 = load i32, ptr %5, align 4, !dbg !105
  %30 = add nsw i32 %29, 1, !dbg !105
  store i32 %30, ptr %5, align 4, !dbg !105
  br label %10, !dbg !90, !llvm.loop !106

31:                                               ; preds = %10
  %32 = load i32, ptr %6, align 4, !dbg !173
  ret i32 %32, !dbg !174
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !36 !pallas.exprWrapper !175 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !35, metadata !DIExpression()), !dbg !176
  call void @llvm.dbg.value(metadata i32 %1, metadata !42, metadata !DIExpression()), !dbg !176
  %3 = icmp ne ptr %0, null, !dbg !177
  br i1 %3, label %4, label %6, !dbg !178

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 0, !dbg !179
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !176
  ret i1 %7, !dbg !176
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !48 !pallas.exprWrapper !175 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !47, metadata !DIExpression()), !dbg !180
  call void @llvm.dbg.value(metadata i32 %1, metadata !50, metadata !DIExpression()), !dbg !180
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !181
  %4 = sext i32 %1 to i64, !dbg !182
  %5 = icmp eq i64 %3, %4, !dbg !183
  ret i1 %5, !dbg !180
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !56 !pallas.exprWrapper !175 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !55, metadata !DIExpression()), !dbg !184
  call void @llvm.dbg.value(metadata i32 %1, metadata !58, metadata !DIExpression()), !dbg !184
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !185
  %5 = icmp sle i32 0, %4, !dbg !186
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !187
  %7 = icmp slt i32 %6, %1, !dbg !188
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !189
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !190
  %10 = sext i32 %9 to i64, !dbg !191
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !191
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !192
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !193
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !194
  ret i1 %13, !dbg !184
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1) #0 !dbg !64 !pallas.exprWrapper !175 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !63, metadata !DIExpression()), !dbg !195
  call void @llvm.dbg.value(metadata i32 %1, metadata !66, metadata !DIExpression()), !dbg !195
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !196
  %4 = sext i32 %1 to i64, !dbg !197
  %5 = icmp eq i64 %3, %4, !dbg !198
  ret i1 %5, !dbg !195
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1) #0 !dbg !72 !pallas.exprWrapper !175 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !71, metadata !DIExpression()), !dbg !199
  call void @llvm.dbg.value(metadata i32 %1, metadata !74, metadata !DIExpression()), !dbg !199
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !200
  %5 = icmp sle i32 0, %4, !dbg !201
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !202
  %7 = icmp slt i32 %6, %1, !dbg !203
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !204
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !205
  %10 = sext i32 %9 to i64, !dbg !206
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !206
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 2), !dbg !207
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !208
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !209
  ret i1 %13, !dbg !199
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1) #0 !dbg !80 !pallas.exprWrapper !175 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !79, metadata !DIExpression()), !dbg !210
  call void @llvm.dbg.value(metadata i32 %1, metadata !82, metadata !DIExpression()), !dbg !210
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !211
  %4 = icmp sle i32 0, %3, !dbg !212
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !213
  %6 = icmp slt i32 %5, %1, !dbg !214
  %7 = call i1 @pallas.scAnd(i1 %4, i1 %6), !dbg !215
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !216
  %9 = sext i32 %8 to i64, !dbg !217
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !217
  %11 = load i32, ptr %10, align 4, !dbg !217
  %12 = call i32 @"pallas.result i32"(), !dbg !218
  %13 = icmp sge i32 %11, %12, !dbg !219
  %14 = call i1 @pallas.forall(i1 %7, i1 %13), !dbg !220
  ret i1 %14, !dbg !210
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !130 !pallas.exprWrapper !175 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !129, metadata !DIExpression()), !dbg !221
  call void @llvm.dbg.value(metadata i32 %1, metadata !132, metadata !DIExpression()), !dbg !221
  call void @llvm.dbg.value(metadata i32 %2, metadata !134, metadata !DIExpression()), !dbg !221
  call void @llvm.dbg.value(metadata i32 %3, metadata !136, metadata !DIExpression()), !dbg !221
  %5 = icmp sle i32 1, %2, !dbg !222
  %6 = icmp sle i32 %2, %1, !dbg !223
  %7 = call i1 @pallas.scAnd(i1 %5, i1 %6), !dbg !224
  ret i1 %7, !dbg !221
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !116 !pallas.exprWrapper !175 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !115, metadata !DIExpression()), !dbg !225
  call void @llvm.dbg.value(metadata i32 %1, metadata !120, metadata !DIExpression()), !dbg !225
  call void @llvm.dbg.value(metadata i32 %2, metadata !122, metadata !DIExpression()), !dbg !225
  call void @llvm.dbg.value(metadata i32 %3, metadata !124, metadata !DIExpression()), !dbg !225
  %5 = icmp ne ptr %0, null, !dbg !226
  ret i1 %5, !dbg !225
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !154 !pallas.exprWrapper !175 {
  %5 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !153, metadata !DIExpression()), !dbg !227
  call void @llvm.dbg.value(metadata i32 %1, metadata !156, metadata !DIExpression()), !dbg !227
  call void @llvm.dbg.value(metadata i32 %2, metadata !158, metadata !DIExpression()), !dbg !227
  call void @llvm.dbg.value(metadata i32 %3, metadata !160, metadata !DIExpression()), !dbg !227
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !228
  %7 = icmp sle i32 0, %6, !dbg !229
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !230
  %9 = icmp slt i32 %8, %1, !dbg !231
  %10 = call i1 @pallas.scAnd(i1 %7, i1 %9), !dbg !232
  %11 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !233
  %12 = sext i32 %11 to i64, !dbg !234
  %13 = getelementptr inbounds i32, ptr %0, i64 %12, !dbg !234
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %5, i32 noundef 1, i32 noundef 2), !dbg !235
  %14 = call i1 @pallas.perm(ptr noundef %13, ptr noundef byval(%pallas.fracT) %5), !dbg !236
  %15 = call i1 @pallas.forallSep(i1 %10, i1 %14), !dbg !237
  ret i1 %15, !dbg !227
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !166 !pallas.exprWrapper !175 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !165, metadata !DIExpression()), !dbg !238
  call void @llvm.dbg.value(metadata i32 %1, metadata !168, metadata !DIExpression()), !dbg !238
  call void @llvm.dbg.value(metadata i32 %2, metadata !170, metadata !DIExpression()), !dbg !238
  call void @llvm.dbg.value(metadata i32 %3, metadata !172, metadata !DIExpression()), !dbg !238
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !239
  %6 = icmp sle i32 0, %5, !dbg !240
  %7 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !241
  %8 = icmp slt i32 %7, %2, !dbg !242
  %9 = call i1 @pallas.scAnd(i1 %6, i1 %8), !dbg !243
  %10 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !244
  %11 = sext i32 %10 to i64, !dbg !245
  %12 = getelementptr inbounds i32, ptr %0, i64 %11, !dbg !245
  %13 = load i32, ptr %12, align 4, !dbg !245
  %14 = icmp sge i32 %13, %3, !dbg !246
  %15 = call i1 @pallas.forall(i1 %9, i1 %14), !dbg !247
  ret i1 %15, !dbg !238
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !142 !pallas.exprWrapper !175 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !141, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %1, metadata !144, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %2, metadata !146, metadata !DIExpression()), !dbg !248
  call void @llvm.dbg.value(metadata i32 %3, metadata !148, metadata !DIExpression()), !dbg !248
  %5 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !249
  %6 = sext i32 %1 to i64, !dbg !250
  %7 = icmp eq i64 %5, %6, !dbg !251
  ret i1 %7, !dbg !248
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !252 i32 @"pallas.result i32"()

declare !pallas.specLib !253 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !254 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !255 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !256 i1 @pallas.forall(i1, i1)

declare !pallas.specLib !257 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !258 i32 @"pallas.boundVar i32"(ptr)

declare !pallas.specLib !259 i64 @pallas.ptrLength(ptr noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!7, !9}
!llvm.module.flags = !{!13, !14, !15, !16, !17, !18, !19}
!llvm.ident = !{!20, !20}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 36, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "6f4068698c67e8997e819001a630118d")
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
!27 = !{!28, i1 false, i1 false, !26, !26, !30, !43, !51, !59, !67, !75}
!28 = !{!"pallas.srcLoc", i64 8, i64 1, i64 18, i64 1, !29}
!29 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_lower_bound.c", directory: "", checksumkind: CSK_MD5, checksum: "efcb7277c13ae93349fe27ab9ec2fd39")
!30 = !{!"pallas.requires", !31, ptr @PALLAS_SPEC_0, !26, !26, !32}
!31 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 30, !29}
!32 = !{!33, !40}
!33 = !{!34, !35}
!34 = !DILocalVariable(name: "arr", arg: 1, scope: !21, file: !8, line: 19, type: !25)
!35 = !DILocalVariable(name: "arr", arg: 1, scope: !36, file: !8, line: 9, type: !25)
!36 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !8, file: !8, line: 9, type: !37, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!37 = !DISubroutineType(types: !38)
!38 = !{!39, !25, !24}
!39 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!40 = !{!41, !42}
!41 = !DILocalVariable(name: "n", arg: 2, scope: !21, file: !8, line: 19, type: !24)
!42 = !DILocalVariable(name: "n", arg: 2, scope: !36, file: !8, line: 9, type: !24)
!43 = !{!"pallas.requires", !44, ptr @PALLAS_SPEC_1, !26, !26, !45}
!44 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 31, !29}
!45 = !{!46, !49}
!46 = !{!34, !47}
!47 = !DILocalVariable(name: "arr", arg: 1, scope: !48, file: !8, line: 10, type: !25)
!48 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !8, file: !8, line: 10, type: !37, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!49 = !{!41, !50}
!50 = !DILocalVariable(name: "n", arg: 2, scope: !48, file: !8, line: 10, type: !24)
!51 = !{!"pallas.requires", !52, ptr @PALLAS_SPEC_2, !26, !26, !53}
!52 = !{!"pallas.srcLoc", i64 11, i64 1, i64 12, i64 87, !29}
!53 = !{!54, !57}
!54 = !{!34, !55}
!55 = !DILocalVariable(name: "arr", arg: 1, scope: !56, file: !8, line: 11, type: !25)
!56 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !8, file: !8, line: 11, type: !37, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!57 = !{!41, !58}
!58 = !DILocalVariable(name: "n", arg: 2, scope: !56, file: !8, line: 11, type: !24)
!59 = !{!"pallas.ensures", !60, ptr @PALLAS_SPEC_3, !26, !26, !61}
!60 = !{!"pallas.srcLoc", i64 13, i64 1, i64 13, i64 30, !29}
!61 = !{!62, !65}
!62 = !{!34, !63}
!63 = !DILocalVariable(name: "arr", arg: 1, scope: !64, file: !8, line: 13, type: !25)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !8, file: !8, line: 13, type: !37, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!65 = !{!41, !66}
!66 = !DILocalVariable(name: "n", arg: 2, scope: !64, file: !8, line: 13, type: !24)
!67 = !{!"pallas.ensures", !68, ptr @PALLAS_SPEC_4, !26, !26, !69}
!68 = !{!"pallas.srcLoc", i64 14, i64 1, i64 15, i64 86, !29}
!69 = !{!70, !73}
!70 = !{!34, !71}
!71 = !DILocalVariable(name: "arr", arg: 1, scope: !72, file: !8, line: 14, type: !25)
!72 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !8, file: !8, line: 14, type: !37, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!73 = !{!41, !74}
!74 = !DILocalVariable(name: "n", arg: 2, scope: !72, file: !8, line: 14, type: !24)
!75 = !{!"pallas.ensures", !76, ptr @PALLAS_SPEC_5, !26, !26, !77}
!76 = !{!"pallas.srcLoc", i64 16, i64 1, i64 17, i64 78, !29}
!77 = !{!78, !81}
!78 = !{!34, !79}
!79 = !DILocalVariable(name: "arr", arg: 1, scope: !80, file: !8, line: 16, type: !25)
!80 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !8, file: !8, line: 16, type: !37, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!81 = !{!41, !82}
!82 = !DILocalVariable(name: "n", arg: 2, scope: !80, file: !8, line: 16, type: !24)
!83 = !DILocation(line: 19, column: 14, scope: !21)
!84 = !DILocation(line: 19, column: 23, scope: !21)
!85 = !DILocalVariable(name: "idx", scope: !21, file: !8, line: 20, type: !24)
!86 = !DILocation(line: 20, column: 9, scope: !21)
!87 = !DILocalVariable(name: "min", scope: !21, file: !8, line: 21, type: !24)
!88 = !DILocation(line: 21, column: 9, scope: !21)
!89 = !DILocation(line: 21, column: 15, scope: !21)
!90 = !DILocation(line: 32, column: 5, scope: !21)
!91 = !DILocation(line: 32, column: 12, scope: !21)
!92 = !DILocation(line: 32, column: 18, scope: !21)
!93 = !DILocation(line: 32, column: 16, scope: !21)
!94 = !DILocation(line: 33, column: 13, scope: !95)
!95 = distinct !DILexicalBlock(scope: !96, file: !8, line: 33, column: 13)
!96 = distinct !DILexicalBlock(scope: !21, file: !8, line: 32, column: 21)
!97 = !DILocation(line: 33, column: 17, scope: !95)
!98 = !DILocation(line: 33, column: 24, scope: !95)
!99 = !DILocation(line: 33, column: 22, scope: !95)
!100 = !DILocation(line: 33, column: 13, scope: !96)
!101 = !DILocation(line: 34, column: 19, scope: !95)
!102 = !DILocation(line: 34, column: 23, scope: !95)
!103 = !DILocation(line: 34, column: 17, scope: !95)
!104 = !DILocation(line: 34, column: 13, scope: !95)
!105 = !DILocation(line: 35, column: 12, scope: !96)
!106 = distinct !{!106, !90, !107, !108, !109}
!107 = !DILocation(line: 36, column: 5, scope: !21)
!108 = !{!"llvm.loop.mustprogress"}
!109 = !{!"pallas.loopInvBlock", !110, !111, !125, !137, !149, !161}
!110 = !{!"pallas.srcLoc", i64 23, i64 5, i64 31, i64 5, !29}
!111 = !{!"pallas.loopInv", !112, ptr @PALLAS_SPEC_6, !26, !26, !113}
!112 = !{!"pallas.srcLoc", i64 24, i64 5, i64 24, i64 31, !29}
!113 = !{!114, !119, !121, !123}
!114 = !{!34, !115}
!115 = !DILocalVariable(name: "arr", arg: 1, scope: !116, file: !8, line: 24, type: !25)
!116 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !8, file: !8, line: 24, type: !117, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!117 = !DISubroutineType(types: !118)
!118 = !{!39, !25, !24, !24, !24}
!119 = !{!41, !120}
!120 = !DILocalVariable(name: "n", arg: 2, scope: !116, file: !8, line: 24, type: !24)
!121 = !{!85, !122}
!122 = !DILocalVariable(name: "idx", arg: 3, scope: !116, file: !8, line: 24, type: !24)
!123 = !{!87, !124}
!124 = !DILocalVariable(name: "min", arg: 4, scope: !116, file: !8, line: 24, type: !24)
!125 = !{!"pallas.loopInv", !126, ptr @PALLAS_SPEC_7, !26, !26, !127}
!126 = !{!"pallas.srcLoc", i64 25, i64 5, i64 25, i64 44, !29}
!127 = !{!128, !131, !133, !135}
!128 = !{!34, !129}
!129 = !DILocalVariable(name: "arr", arg: 1, scope: !130, file: !8, line: 25, type: !25)
!130 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !8, file: !8, line: 25, type: !117, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!131 = !{!41, !132}
!132 = !DILocalVariable(name: "n", arg: 2, scope: !130, file: !8, line: 25, type: !24)
!133 = !{!85, !134}
!134 = !DILocalVariable(name: "idx", arg: 3, scope: !130, file: !8, line: 25, type: !24)
!135 = !{!87, !136}
!136 = !DILocalVariable(name: "min", arg: 4, scope: !130, file: !8, line: 25, type: !24)
!137 = !{!"pallas.loopInv", !138, ptr @PALLAS_SPEC_8, !26, !26, !139}
!138 = !{!"pallas.srcLoc", i64 26, i64 5, i64 26, i64 41, !29}
!139 = !{!140, !143, !145, !147}
!140 = !{!34, !141}
!141 = !DILocalVariable(name: "arr", arg: 1, scope: !142, file: !8, line: 26, type: !25)
!142 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !8, file: !8, line: 26, type: !117, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!143 = !{!41, !144}
!144 = !DILocalVariable(name: "n", arg: 2, scope: !142, file: !8, line: 26, type: !24)
!145 = !{!85, !146}
!146 = !DILocalVariable(name: "idx", arg: 3, scope: !142, file: !8, line: 26, type: !24)
!147 = !{!87, !148}
!148 = !DILocalVariable(name: "min", arg: 4, scope: !142, file: !8, line: 26, type: !24)
!149 = !{!"pallas.loopInv", !150, ptr @PALLAS_SPEC_9, !26, !26, !151}
!150 = !{!"pallas.srcLoc", i64 27, i64 5, i64 28, i64 97, !29}
!151 = !{!152, !155, !157, !159}
!152 = !{!34, !153}
!153 = !DILocalVariable(name: "arr", arg: 1, scope: !154, file: !8, line: 27, type: !25)
!154 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !8, file: !8, line: 27, type: !117, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!155 = !{!41, !156}
!156 = !DILocalVariable(name: "n", arg: 2, scope: !154, file: !8, line: 27, type: !24)
!157 = !{!85, !158}
!158 = !DILocalVariable(name: "idx", arg: 3, scope: !154, file: !8, line: 27, type: !24)
!159 = !{!87, !160}
!160 = !DILocalVariable(name: "min", arg: 4, scope: !154, file: !8, line: 27, type: !24)
!161 = !{!"pallas.loopInv", !162, ptr @PALLAS_SPEC_10, !26, !26, !163}
!162 = !{!"pallas.srcLoc", i64 29, i64 5, i64 30, i64 82, !29}
!163 = !{!164, !167, !169, !171}
!164 = !{!34, !165}
!165 = !DILocalVariable(name: "arr", arg: 1, scope: !166, file: !8, line: 29, type: !25)
!166 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !8, file: !8, line: 29, type: !117, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!167 = !{!41, !168}
!168 = !DILocalVariable(name: "n", arg: 2, scope: !166, file: !8, line: 29, type: !24)
!169 = !{!85, !170}
!170 = !DILocalVariable(name: "idx", arg: 3, scope: !166, file: !8, line: 29, type: !24)
!171 = !{!87, !172}
!172 = !DILocalVariable(name: "min", arg: 4, scope: !166, file: !8, line: 29, type: !24)
!173 = !DILocation(line: 38, column: 12, scope: !21)
!174 = !DILocation(line: 38, column: 5, scope: !21)
!175 = !{!""}
!176 = !DILocation(line: 0, scope: !36)
!177 = !DILocation(line: 9, column: 14, scope: !36)
!178 = !DILocation(line: 9, column: 22, scope: !36)
!179 = !DILocation(line: 9, column: 27, scope: !36)
!180 = !DILocation(line: 0, scope: !48)
!181 = !DILocation(line: 10, column: 10, scope: !48)
!182 = !DILocation(line: 10, column: 30, scope: !48)
!183 = !DILocation(line: 10, column: 27, scope: !48)
!184 = !DILocation(line: 0, scope: !56)
!185 = !DILocation(line: 11, column: 29, scope: !56)
!186 = !DILocation(line: 11, column: 26, scope: !56)
!187 = !DILocation(line: 12, column: 29, scope: !56)
!188 = !DILocation(line: 12, column: 41, scope: !56)
!189 = !DILocation(line: 11, column: 19, scope: !56)
!190 = !DILocation(line: 12, column: 58, scope: !56)
!191 = !DILocation(line: 12, column: 54, scope: !56)
!192 = !DILocation(line: 12, column: 72, scope: !56)
!193 = !DILocation(line: 12, column: 47, scope: !56)
!194 = !DILocation(line: 11, column: 10, scope: !56)
!195 = !DILocation(line: 0, scope: !64)
!196 = !DILocation(line: 13, column: 9, scope: !64)
!197 = !DILocation(line: 13, column: 29, scope: !64)
!198 = !DILocation(line: 13, column: 26, scope: !64)
!199 = !DILocation(line: 0, scope: !72)
!200 = !DILocation(line: 14, column: 28, scope: !72)
!201 = !DILocation(line: 14, column: 25, scope: !72)
!202 = !DILocation(line: 15, column: 28, scope: !72)
!203 = !DILocation(line: 15, column: 40, scope: !72)
!204 = !DILocation(line: 14, column: 18, scope: !72)
!205 = !DILocation(line: 15, column: 57, scope: !72)
!206 = !DILocation(line: 15, column: 53, scope: !72)
!207 = !DILocation(line: 15, column: 71, scope: !72)
!208 = !DILocation(line: 15, column: 46, scope: !72)
!209 = !DILocation(line: 14, column: 9, scope: !72)
!210 = !DILocation(line: 0, scope: !80)
!211 = !DILocation(line: 16, column: 27, scope: !80)
!212 = !DILocation(line: 16, column: 24, scope: !80)
!213 = !DILocation(line: 17, column: 27, scope: !80)
!214 = !DILocation(line: 17, column: 39, scope: !80)
!215 = !DILocation(line: 16, column: 17, scope: !80)
!216 = !DILocation(line: 17, column: 49, scope: !80)
!217 = !DILocation(line: 17, column: 45, scope: !80)
!218 = !DILocation(line: 17, column: 65, scope: !80)
!219 = !DILocation(line: 17, column: 62, scope: !80)
!220 = !DILocation(line: 16, column: 9, scope: !80)
!221 = !DILocation(line: 0, scope: !130)
!222 = !DILocation(line: 25, column: 27, scope: !130)
!223 = !DILocation(line: 25, column: 39, scope: !130)
!224 = !DILocation(line: 25, column: 20, scope: !130)
!225 = !DILocation(line: 0, scope: !116)
!226 = !DILocation(line: 24, column: 24, scope: !116)
!227 = !DILocation(line: 0, scope: !154)
!228 = !DILocation(line: 27, column: 39, scope: !154)
!229 = !DILocation(line: 27, column: 36, scope: !154)
!230 = !DILocation(line: 28, column: 39, scope: !154)
!231 = !DILocation(line: 28, column: 51, scope: !154)
!232 = !DILocation(line: 27, column: 29, scope: !154)
!233 = !DILocation(line: 28, column: 68, scope: !154)
!234 = !DILocation(line: 28, column: 64, scope: !154)
!235 = !DILocation(line: 28, column: 82, scope: !154)
!236 = !DILocation(line: 28, column: 57, scope: !154)
!237 = !DILocation(line: 27, column: 20, scope: !154)
!238 = !DILocation(line: 0, scope: !166)
!239 = !DILocation(line: 29, column: 38, scope: !166)
!240 = !DILocation(line: 29, column: 35, scope: !166)
!241 = !DILocation(line: 30, column: 38, scope: !166)
!242 = !DILocation(line: 30, column: 50, scope: !166)
!243 = !DILocation(line: 29, column: 28, scope: !166)
!244 = !DILocation(line: 30, column: 62, scope: !166)
!245 = !DILocation(line: 30, column: 58, scope: !166)
!246 = !DILocation(line: 30, column: 75, scope: !166)
!247 = !DILocation(line: 29, column: 20, scope: !166)
!248 = !DILocation(line: 0, scope: !142)
!249 = !DILocation(line: 26, column: 20, scope: !142)
!250 = !DILocation(line: 26, column: 40, scope: !142)
!251 = !DILocation(line: 26, column: 37, scope: !142)
!252 = !{!"pallas.result"}
!253 = !{!"pallas.forallSep"}
!254 = !{!"pallas.perm"}
!255 = !{!"pallas.fracOf"}
!256 = !{!"pallas.forall"}
!257 = !{!"pallas.scAnd"}
!258 = !{!"pallas.boundVar"}
!259 = !{!"pallas.ptrLength"}
