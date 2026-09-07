; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_quantifier.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [12 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0

; Function Attrs: noinline nounwind uwtable
define dso_local void @bar(ptr noundef %0, i32 noundef %1) #0 !dbg !21 !pallas.fcontract !27 {
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
  store i32 0, ptr %8, align 4, !dbg !88
  ret void, !dbg !89
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @foo(ptr noundef %0, i32 noundef %1) #0 !dbg !90 !pallas.fcontract !91 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !97, metadata !DIExpression()), !dbg !143
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !101, metadata !DIExpression()), !dbg !144
  %5 = load ptr, ptr %3, align 8, !dbg !145
  %6 = getelementptr inbounds i32, ptr %5, i64 0, !dbg !145
  store i32 0, ptr %6, align 4, !dbg !146
  ret void, !dbg !147
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !36 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !35, metadata !DIExpression()), !dbg !149
  call void @llvm.dbg.value(metadata i32 %1, metadata !42, metadata !DIExpression()), !dbg !149
  %3 = icmp ne ptr %0, null, !dbg !150
  br i1 %3, label %4, label %6, !dbg !151

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 2, !dbg !152
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !149
  ret i1 %7, !dbg !149
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !48 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !47, metadata !DIExpression()), !dbg !153
  call void @llvm.dbg.value(metadata i32 %1, metadata !50, metadata !DIExpression()), !dbg !153
  %3 = call i64 @pallas.ptrBlockLength(ptr noundef %0), !dbg !154
  %4 = sext i32 %1 to i64, !dbg !155
  %5 = icmp eq i64 %3, %4, !dbg !156
  br i1 %5, label %6, label %9, !dbg !157

6:                                                ; preds = %2
  %7 = call i64 @pallas.ptrBlockOffset(ptr noundef %0), !dbg !158
  %8 = icmp eq i64 %7, 0, !dbg !159
  br label %9

9:                                                ; preds = %6, %2
  %10 = phi i1 [ false, %2 ], [ %8, %6 ], !dbg !153
  ret i1 %10, !dbg !153
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !56 !pallas.exprWrapper !148 {
  %3 = alloca %pallas.fracT, align 8
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !55, metadata !DIExpression()), !dbg !160
  call void @llvm.dbg.value(metadata i32 %1, metadata !58, metadata !DIExpression()), !dbg !160
  %5 = getelementptr inbounds i32, ptr %0, i64 0, !dbg !161
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !162
  %6 = call i1 @pallas.perm(ptr noundef %5, ptr noundef byval(%pallas.fracT) %3), !dbg !163
  %7 = getelementptr inbounds i32, ptr %0, i64 1, !dbg !164
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !165
  %8 = call i1 @pallas.perm(ptr noundef %7, ptr noundef byval(%pallas.fracT) %4), !dbg !166
  %9 = call i1 @pallas.sepConj(i1 %6, i1 %8), !dbg !167
  ret i1 %9, !dbg !160
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1) #0 !dbg !64 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !63, metadata !DIExpression()), !dbg !168
  call void @llvm.dbg.value(metadata i32 %1, metadata !66, metadata !DIExpression()), !dbg !168
  %3 = call i64 @pallas.ptrBlockLength(ptr noundef %0), !dbg !169
  %4 = sext i32 %1 to i64, !dbg !170
  %5 = icmp eq i64 %3, %4, !dbg !171
  br i1 %5, label %6, label %9, !dbg !172

6:                                                ; preds = %2
  %7 = call i64 @pallas.ptrBlockOffset(ptr noundef %0), !dbg !173
  %8 = icmp eq i64 %7, 0, !dbg !174
  br label %9

9:                                                ; preds = %6, %2
  %10 = phi i1 [ false, %2 ], [ %8, %6 ], !dbg !168
  ret i1 %10, !dbg !168
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1) #0 !dbg !72 !pallas.exprWrapper !148 {
  %3 = alloca %pallas.fracT, align 8
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !71, metadata !DIExpression()), !dbg !175
  call void @llvm.dbg.value(metadata i32 %1, metadata !74, metadata !DIExpression()), !dbg !175
  %5 = getelementptr inbounds i32, ptr %0, i64 0, !dbg !176
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !177
  %6 = call i1 @pallas.perm(ptr noundef %5, ptr noundef byval(%pallas.fracT) %3), !dbg !178
  %7 = getelementptr inbounds i32, ptr %0, i64 1, !dbg !179
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !180
  %8 = call i1 @pallas.perm(ptr noundef %7, ptr noundef byval(%pallas.fracT) %4), !dbg !181
  %9 = call i1 @pallas.sepConj(i1 %6, i1 %8), !dbg !182
  ret i1 %9, !dbg !175
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1) #0 !dbg !80 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !79, metadata !DIExpression()), !dbg !183
  call void @llvm.dbg.value(metadata i32 %1, metadata !82, metadata !DIExpression()), !dbg !183
  %3 = getelementptr inbounds i32, ptr %0, i64 0, !dbg !184
  %4 = load i32, ptr %3, align 4, !dbg !184
  %5 = icmp eq i32 %4, 0, !dbg !185
  br i1 %5, label %6, label %10, !dbg !186

6:                                                ; preds = %2
  %7 = getelementptr inbounds i32, ptr %0, i64 1, !dbg !187
  %8 = load i32, ptr %7, align 4, !dbg !187
  %9 = icmp eq i32 %8, 0, !dbg !188
  br label %10

10:                                               ; preds = %6, %2
  %11 = phi i1 [ false, %2 ], [ %9, %6 ], !dbg !183
  ret i1 %11, !dbg !183
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1) #0 !dbg !99 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !98, metadata !DIExpression()), !dbg !189
  call void @llvm.dbg.value(metadata i32 %1, metadata !102, metadata !DIExpression()), !dbg !189
  %3 = icmp ne ptr %0, null, !dbg !190
  br i1 %3, label %4, label %6, !dbg !191

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 2, !dbg !192
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !189
  ret i1 %7, !dbg !189
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1) #0 !dbg !108 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !107, metadata !DIExpression()), !dbg !193
  call void @llvm.dbg.value(metadata i32 %1, metadata !110, metadata !DIExpression()), !dbg !193
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !194
  %4 = sext i32 %1 to i64, !dbg !195
  %5 = icmp eq i64 %3, %4, !dbg !196
  ret i1 %5, !dbg !193
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1) #0 !dbg !116 !pallas.exprWrapper !148 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !115, metadata !DIExpression()), !dbg !197
  call void @llvm.dbg.value(metadata i32 %1, metadata !118, metadata !DIExpression()), !dbg !197
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !198
  %5 = icmp sle i32 0, %4, !dbg !199
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !200
  %7 = icmp slt i32 %6, %1, !dbg !201
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !202
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !203
  %10 = sext i32 %9 to i64, !dbg !204
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !204
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !205
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !206
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !207
  ret i1 %13, !dbg !197
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0, i32 noundef %1) #0 !dbg !124 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !123, metadata !DIExpression()), !dbg !208
  call void @llvm.dbg.value(metadata i32 %1, metadata !126, metadata !DIExpression()), !dbg !208
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !209
  %4 = sext i32 %1 to i64, !dbg !210
  %5 = icmp eq i64 %3, %4, !dbg !211
  ret i1 %5, !dbg !208
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0, i32 noundef %1) #0 !dbg !132 !pallas.exprWrapper !148 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !131, metadata !DIExpression()), !dbg !212
  call void @llvm.dbg.value(metadata i32 %1, metadata !134, metadata !DIExpression()), !dbg !212
  %4 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !213
  %5 = icmp sle i32 0, %4, !dbg !214
  %6 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !215
  %7 = icmp slt i32 %6, %1, !dbg !216
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !217
  %9 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !218
  %10 = sext i32 %9 to i64, !dbg !219
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !219
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !220
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !221
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !222
  ret i1 %13, !dbg !212
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0, i32 noundef %1) #0 !dbg !140 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !139, metadata !DIExpression()), !dbg !223
  call void @llvm.dbg.value(metadata i32 %1, metadata !142, metadata !DIExpression()), !dbg !223
  %3 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !224
  %4 = icmp sle i32 0, %3, !dbg !225
  %5 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !226
  %6 = icmp slt i32 %5, %1, !dbg !227
  %7 = call i1 @pallas.scAnd(i1 %4, i1 %6), !dbg !228
  %8 = call i32 @"pallas.boundVar i32"(ptr @.str), !dbg !229
  %9 = sext i32 %8 to i64, !dbg !230
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !230
  %11 = load i32, ptr %10, align 4, !dbg !230
  %12 = icmp eq i32 %11, 0, !dbg !231
  %13 = call i1 @pallas.exists(i1 %7, i1 %12), !dbg !232
  ret i1 %13, !dbg !223
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !233 i64 @pallas.ptrBlockOffset(ptr noundef)

declare !pallas.specLib !234 i64 @pallas.ptrBlockLength(ptr noundef)

declare !pallas.specLib !235 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !236 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !237 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !238 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !239 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !240 i1 @pallas.exists(i1, i1)

declare !pallas.specLib !241 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !242 i32 @"pallas.boundVar i32"(ptr)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!7, !9}
!llvm.module.flags = !{!13, !14, !15, !16, !17, !18, !19}
!llvm.ident = !{!20, !20}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 107, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "2e9ba772600a9b3a9b50b622dfd5eeb6")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = distinct !DICompileUnit(language: DW_LANG_C11, file: !8, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!8 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_quantifier.c", directory: ".", checksumkind: CSK_MD5, checksum: "d271dd2a32c4ac974e6455a36334b368")
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
!21 = distinct !DISubprogram(name: "bar", scope: !8, file: !8, line: 17, type: !22, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!22 = !DISubroutineType(types: !23)
!23 = !{null, !24, !25}
!24 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !25, size: 64)
!25 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!26 = !{}
!27 = !{!28, i1 false, i1 false, !26, !26, !30, !43, !51, !59, !67, !75}
!28 = !{!"pallas.srcLoc", i64 7, i64 1, i64 16, i64 1, !29}
!29 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_quantifier.c", directory: "", checksumkind: CSK_MD5, checksum: "d271dd2a32c4ac974e6455a36334b368")
!30 = !{!"pallas.requires", !31, ptr @PALLAS_SPEC_0, !26, !26, !32}
!31 = !{!"pallas.srcLoc", i64 8, i64 1, i64 8, i64 30, !29}
!32 = !{!33, !40}
!33 = !{!34, !35}
!34 = !DILocalVariable(name: "arr", arg: 1, scope: !21, file: !8, line: 17, type: !24)
!35 = !DILocalVariable(name: "arr", arg: 1, scope: !36, file: !8, line: 8, type: !24)
!36 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !8, file: !8, line: 8, type: !37, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!37 = !DISubroutineType(types: !38)
!38 = !{!39, !24, !25}
!39 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!40 = !{!41, !42}
!41 = !DILocalVariable(name: "n", arg: 2, scope: !21, file: !8, line: 17, type: !25)
!42 = !DILocalVariable(name: "n", arg: 2, scope: !36, file: !8, line: 8, type: !25)
!43 = !{!"pallas.requires", !44, ptr @PALLAS_SPEC_1, !26, !26, !45}
!44 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 68, !29}
!45 = !{!46, !49}
!46 = !{!34, !47}
!47 = !DILocalVariable(name: "arr", arg: 1, scope: !48, file: !8, line: 9, type: !24)
!48 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !8, file: !8, line: 9, type: !37, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!49 = !{!41, !50}
!50 = !DILocalVariable(name: "n", arg: 2, scope: !48, file: !8, line: 9, type: !25)
!51 = !{!"pallas.requires", !52, ptr @PALLAS_SPEC_2, !26, !26, !53}
!52 = !{!"pallas.srcLoc", i64 10, i64 1, i64 11, i64 36, !29}
!53 = !{!54, !57}
!54 = !{!34, !55}
!55 = !DILocalVariable(name: "arr", arg: 1, scope: !56, file: !8, line: 10, type: !24)
!56 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !8, file: !8, line: 10, type: !37, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!57 = !{!41, !58}
!58 = !DILocalVariable(name: "n", arg: 2, scope: !56, file: !8, line: 10, type: !25)
!59 = !{!"pallas.ensures", !60, ptr @PALLAS_SPEC_3, !26, !26, !61}
!60 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 67, !29}
!61 = !{!62, !65}
!62 = !{!34, !63}
!63 = !DILocalVariable(name: "arr", arg: 1, scope: !64, file: !8, line: 12, type: !24)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !8, file: !8, line: 12, type: !37, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!65 = !{!41, !66}
!66 = !DILocalVariable(name: "n", arg: 2, scope: !64, file: !8, line: 12, type: !25)
!67 = !{!"pallas.ensures", !68, ptr @PALLAS_SPEC_4, !26, !26, !69}
!68 = !{!"pallas.srcLoc", i64 13, i64 1, i64 14, i64 35, !29}
!69 = !{!70, !73}
!70 = !{!34, !71}
!71 = !DILocalVariable(name: "arr", arg: 1, scope: !72, file: !8, line: 13, type: !24)
!72 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !8, file: !8, line: 13, type: !37, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!73 = !{!41, !74}
!74 = !DILocalVariable(name: "n", arg: 2, scope: !72, file: !8, line: 13, type: !25)
!75 = !{!"pallas.ensures", !76, ptr @PALLAS_SPEC_5, !26, !26, !77}
!76 = !{!"pallas.srcLoc", i64 15, i64 1, i64 15, i64 35, !29}
!77 = !{!78, !81}
!78 = !{!34, !79}
!79 = !DILocalVariable(name: "arr", arg: 1, scope: !80, file: !8, line: 15, type: !24)
!80 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !8, file: !8, line: 15, type: !37, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!81 = !{!41, !82}
!82 = !DILocalVariable(name: "n", arg: 2, scope: !80, file: !8, line: 15, type: !25)
!83 = !DILocation(line: 17, column: 15, scope: !21)
!84 = !DILocation(line: 17, column: 24, scope: !21)
!85 = !DILocation(line: 18, column: 5, scope: !21)
!86 = !DILocation(line: 18, column: 12, scope: !21)
!87 = !DILocation(line: 19, column: 5, scope: !21)
!88 = !DILocation(line: 19, column: 12, scope: !21)
!89 = !DILocation(line: 20, column: 1, scope: !21)
!90 = distinct !DISubprogram(name: "foo", scope: !8, file: !8, line: 34, type: !22, scopeLine: 34, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!91 = !{!92, i1 false, i1 false, !26, !26, !93, !103, !111, !119, !127, !135}
!92 = !{!"pallas.srcLoc", i64 23, i64 1, i64 33, i64 1, !29}
!93 = !{!"pallas.requires", !94, ptr @PALLAS_SPEC_6, !26, !26, !95}
!94 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 30, !29}
!95 = !{!96, !100}
!96 = !{!97, !98}
!97 = !DILocalVariable(name: "arr", arg: 1, scope: !90, file: !8, line: 34, type: !24)
!98 = !DILocalVariable(name: "arr", arg: 1, scope: !99, file: !8, line: 24, type: !24)
!99 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !8, file: !8, line: 24, type: !37, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!100 = !{!101, !102}
!101 = !DILocalVariable(name: "n", arg: 2, scope: !90, file: !8, line: 34, type: !25)
!102 = !DILocalVariable(name: "n", arg: 2, scope: !99, file: !8, line: 24, type: !25)
!103 = !{!"pallas.requires", !104, ptr @PALLAS_SPEC_7, !26, !26, !105}
!104 = !{!"pallas.srcLoc", i64 25, i64 1, i64 25, i64 31, !29}
!105 = !{!106, !109}
!106 = !{!97, !107}
!107 = !DILocalVariable(name: "arr", arg: 1, scope: !108, file: !8, line: 25, type: !24)
!108 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !8, file: !8, line: 25, type: !37, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!109 = !{!101, !110}
!110 = !DILocalVariable(name: "n", arg: 2, scope: !108, file: !8, line: 25, type: !25)
!111 = !{!"pallas.requires", !112, ptr @PALLAS_SPEC_8, !26, !26, !113}
!112 = !{!"pallas.srcLoc", i64 26, i64 1, i64 27, i64 80, !29}
!113 = !{!114, !117}
!114 = !{!97, !115}
!115 = !DILocalVariable(name: "arr", arg: 1, scope: !116, file: !8, line: 26, type: !24)
!116 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !8, file: !8, line: 26, type: !37, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!117 = !{!101, !118}
!118 = !DILocalVariable(name: "n", arg: 2, scope: !116, file: !8, line: 26, type: !25)
!119 = !{!"pallas.ensures", !120, ptr @PALLAS_SPEC_9, !26, !26, !121}
!120 = !{!"pallas.srcLoc", i64 28, i64 1, i64 28, i64 30, !29}
!121 = !{!122, !125}
!122 = !{!97, !123}
!123 = !DILocalVariable(name: "arr", arg: 1, scope: !124, file: !8, line: 28, type: !24)
!124 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !8, file: !8, line: 28, type: !37, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!125 = !{!101, !126}
!126 = !DILocalVariable(name: "n", arg: 2, scope: !124, file: !8, line: 28, type: !25)
!127 = !{!"pallas.ensures", !128, ptr @PALLAS_SPEC_10, !26, !26, !129}
!128 = !{!"pallas.srcLoc", i64 29, i64 1, i64 30, i64 79, !29}
!129 = !{!130, !133}
!130 = !{!97, !131}
!131 = !DILocalVariable(name: "arr", arg: 1, scope: !132, file: !8, line: 29, type: !24)
!132 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !8, file: !8, line: 29, type: !37, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!133 = !{!101, !134}
!134 = !DILocalVariable(name: "n", arg: 2, scope: !132, file: !8, line: 29, type: !25)
!135 = !{!"pallas.ensures", !136, ptr @PALLAS_SPEC_11, !26, !26, !137}
!136 = !{!"pallas.srcLoc", i64 31, i64 1, i64 32, i64 67, !29}
!137 = !{!138, !141}
!138 = !{!97, !139}
!139 = !DILocalVariable(name: "arr", arg: 1, scope: !140, file: !8, line: 31, type: !24)
!140 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !8, file: !8, line: 31, type: !37, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !26)
!141 = !{!101, !142}
!142 = !DILocalVariable(name: "n", arg: 2, scope: !140, file: !8, line: 31, type: !25)
!143 = !DILocation(line: 34, column: 15, scope: !90)
!144 = !DILocation(line: 34, column: 24, scope: !90)
!145 = !DILocation(line: 35, column: 5, scope: !90)
!146 = !DILocation(line: 35, column: 12, scope: !90)
!147 = !DILocation(line: 36, column: 1, scope: !90)
!148 = !{!""}
!149 = !DILocation(line: 0, scope: !36)
!150 = !DILocation(line: 8, column: 14, scope: !36)
!151 = !DILocation(line: 8, column: 22, scope: !36)
!152 = !DILocation(line: 8, column: 27, scope: !36)
!153 = !DILocation(line: 0, scope: !48)
!154 = !DILocation(line: 9, column: 10, scope: !48)
!155 = !DILocation(line: 9, column: 36, scope: !48)
!156 = !DILocation(line: 9, column: 33, scope: !48)
!157 = !DILocation(line: 9, column: 38, scope: !48)
!158 = !DILocation(line: 9, column: 41, scope: !48)
!159 = !DILocation(line: 9, column: 64, scope: !48)
!160 = !DILocation(line: 0, scope: !56)
!161 = !DILocation(line: 10, column: 24, scope: !56)
!162 = !DILocation(line: 10, column: 28, scope: !56)
!163 = !DILocation(line: 10, column: 15, scope: !56)
!164 = !DILocation(line: 11, column: 24, scope: !56)
!165 = !DILocation(line: 11, column: 28, scope: !56)
!166 = !DILocation(line: 11, column: 15, scope: !56)
!167 = !DILocation(line: 10, column: 10, scope: !56)
!168 = !DILocation(line: 0, scope: !64)
!169 = !DILocation(line: 12, column: 9, scope: !64)
!170 = !DILocation(line: 12, column: 35, scope: !64)
!171 = !DILocation(line: 12, column: 32, scope: !64)
!172 = !DILocation(line: 12, column: 37, scope: !64)
!173 = !DILocation(line: 12, column: 40, scope: !64)
!174 = !DILocation(line: 12, column: 63, scope: !64)
!175 = !DILocation(line: 0, scope: !72)
!176 = !DILocation(line: 13, column: 23, scope: !72)
!177 = !DILocation(line: 13, column: 27, scope: !72)
!178 = !DILocation(line: 13, column: 14, scope: !72)
!179 = !DILocation(line: 14, column: 23, scope: !72)
!180 = !DILocation(line: 14, column: 27, scope: !72)
!181 = !DILocation(line: 14, column: 14, scope: !72)
!182 = !DILocation(line: 13, column: 9, scope: !72)
!183 = !DILocation(line: 0, scope: !80)
!184 = !DILocation(line: 15, column: 9, scope: !80)
!185 = !DILocation(line: 15, column: 16, scope: !80)
!186 = !DILocation(line: 15, column: 21, scope: !80)
!187 = !DILocation(line: 15, column: 24, scope: !80)
!188 = !DILocation(line: 15, column: 31, scope: !80)
!189 = !DILocation(line: 0, scope: !99)
!190 = !DILocation(line: 24, column: 14, scope: !99)
!191 = !DILocation(line: 24, column: 22, scope: !99)
!192 = !DILocation(line: 24, column: 27, scope: !99)
!193 = !DILocation(line: 0, scope: !108)
!194 = !DILocation(line: 25, column: 10, scope: !108)
!195 = !DILocation(line: 25, column: 30, scope: !108)
!196 = !DILocation(line: 25, column: 27, scope: !108)
!197 = !DILocation(line: 0, scope: !116)
!198 = !DILocation(line: 26, column: 29, scope: !116)
!199 = !DILocation(line: 26, column: 26, scope: !116)
!200 = !DILocation(line: 27, column: 29, scope: !116)
!201 = !DILocation(line: 27, column: 41, scope: !116)
!202 = !DILocation(line: 26, column: 19, scope: !116)
!203 = !DILocation(line: 27, column: 58, scope: !116)
!204 = !DILocation(line: 27, column: 54, scope: !116)
!205 = !DILocation(line: 27, column: 72, scope: !116)
!206 = !DILocation(line: 27, column: 47, scope: !116)
!207 = !DILocation(line: 26, column: 10, scope: !116)
!208 = !DILocation(line: 0, scope: !124)
!209 = !DILocation(line: 28, column: 9, scope: !124)
!210 = !DILocation(line: 28, column: 29, scope: !124)
!211 = !DILocation(line: 28, column: 26, scope: !124)
!212 = !DILocation(line: 0, scope: !132)
!213 = !DILocation(line: 29, column: 28, scope: !132)
!214 = !DILocation(line: 29, column: 25, scope: !132)
!215 = !DILocation(line: 30, column: 28, scope: !132)
!216 = !DILocation(line: 30, column: 40, scope: !132)
!217 = !DILocation(line: 29, column: 18, scope: !132)
!218 = !DILocation(line: 30, column: 57, scope: !132)
!219 = !DILocation(line: 30, column: 53, scope: !132)
!220 = !DILocation(line: 30, column: 71, scope: !132)
!221 = !DILocation(line: 30, column: 46, scope: !132)
!222 = !DILocation(line: 29, column: 9, scope: !132)
!223 = !DILocation(line: 0, scope: !140)
!224 = !DILocation(line: 31, column: 27, scope: !140)
!225 = !DILocation(line: 31, column: 24, scope: !140)
!226 = !DILocation(line: 32, column: 27, scope: !140)
!227 = !DILocation(line: 32, column: 39, scope: !140)
!228 = !DILocation(line: 31, column: 17, scope: !140)
!229 = !DILocation(line: 32, column: 49, scope: !140)
!230 = !DILocation(line: 32, column: 45, scope: !140)
!231 = !DILocation(line: 32, column: 62, scope: !140)
!232 = !DILocation(line: 31, column: 9, scope: !140)
!233 = !{!"pallas.ptrBlockOffset"}
!234 = !{!"pallas.ptrBlockLength"}
!235 = !{!"pallas.sepConj"}
!236 = !{!"pallas.ptrLength"}
!237 = !{!"pallas.forallSep"}
!238 = !{!"pallas.perm"}
!239 = !{!"pallas.fracOf"}
!240 = !{!"pallas.exists"}
!241 = !{!"pallas.scAnd"}
!242 = !{!"pallas.boundVar"}
