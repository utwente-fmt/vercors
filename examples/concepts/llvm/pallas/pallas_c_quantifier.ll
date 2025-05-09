; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_quantifier.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [12 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0

; Function Attrs: noinline nounwind uwtable
define dso_local void @bar(ptr noundef %0, i32 noundef %1) #0 !dbg !22 !pallas.fcontract !28 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !32, metadata !DIExpression()), !dbg !44
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !33, metadata !DIExpression()), !dbg !45
  %5 = load ptr, ptr %3, align 8, !dbg !46
  %6 = getelementptr inbounds i32, ptr %5, i64 0, !dbg !46
  store i32 0, ptr %6, align 4, !dbg !47
  %7 = load ptr, ptr %3, align 8, !dbg !48
  %8 = getelementptr inbounds i32, ptr %7, i64 1, !dbg !48
  store i32 0, ptr %8, align 4, !dbg !49
  ret void, !dbg !50
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @foo(ptr noundef %0, i32 noundef %1) #0 !dbg !51 !pallas.fcontract !52 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  call void @llvm.dbg.declare(metadata ptr %3, metadata !56, metadata !DIExpression()), !dbg !68
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !57, metadata !DIExpression()), !dbg !69
  %5 = load ptr, ptr %3, align 8, !dbg !70
  %6 = getelementptr inbounds i32, ptr %5, i64 0, !dbg !70
  store i32 0, ptr %6, align 4, !dbg !71
  ret void, !dbg !72
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !73 !pallas.exprWrapper !77 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !78, metadata !DIExpression()), !dbg !79
  call void @llvm.dbg.value(metadata i32 %1, metadata !80, metadata !DIExpression()), !dbg !79
  %3 = icmp ne ptr %0, null, !dbg !81
  br i1 %3, label %4, label %6, !dbg !82

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 2, !dbg !83
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !79
  ret i1 %7, !dbg !79
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !84 !pallas.exprWrapper !77 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !85, metadata !DIExpression()), !dbg !86
  call void @llvm.dbg.value(metadata i32 %1, metadata !87, metadata !DIExpression()), !dbg !86
  %3 = call i64 @pallas.ptrBlockLength(ptr noundef %0), !dbg !88
  %4 = sext i32 %1 to i64, !dbg !89
  %5 = icmp eq i64 %3, %4, !dbg !90
  br i1 %5, label %6, label %9, !dbg !91

6:                                                ; preds = %2
  %7 = call i64 @pallas.ptrBlockOffset(ptr noundef %0), !dbg !92
  %8 = icmp eq i64 %7, 0, !dbg !93
  br label %9

9:                                                ; preds = %6, %2
  %10 = phi i1 [ false, %2 ], [ %8, %6 ], !dbg !86
  ret i1 %10, !dbg !86
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !94 !pallas.exprWrapper !77 {
  %3 = alloca %pallas.fracT, align 8
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !95, metadata !DIExpression()), !dbg !96
  call void @llvm.dbg.value(metadata i32 %1, metadata !97, metadata !DIExpression()), !dbg !96
  %5 = getelementptr inbounds i32, ptr %0, i64 0, !dbg !98
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !99
  %6 = call i1 @pallas.perm(ptr noundef %5, ptr noundef byval(%pallas.fracT) %3), !dbg !100
  %7 = getelementptr inbounds i32, ptr %0, i64 1, !dbg !101
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !102
  %8 = call i1 @pallas.perm(ptr noundef %7, ptr noundef byval(%pallas.fracT) %4), !dbg !103
  %9 = call i1 @pallas.sepConj(i1 %6, i1 %8), !dbg !104
  ret i1 %9, !dbg !96
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1) #0 !dbg !105 !pallas.exprWrapper !77 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !106, metadata !DIExpression()), !dbg !107
  call void @llvm.dbg.value(metadata i32 %1, metadata !108, metadata !DIExpression()), !dbg !107
  %3 = call i64 @pallas.ptrBlockLength(ptr noundef %0), !dbg !109
  %4 = sext i32 %1 to i64, !dbg !110
  %5 = icmp eq i64 %3, %4, !dbg !111
  br i1 %5, label %6, label %9, !dbg !112

6:                                                ; preds = %2
  %7 = call i64 @pallas.ptrBlockOffset(ptr noundef %0), !dbg !113
  %8 = icmp eq i64 %7, 0, !dbg !114
  br label %9

9:                                                ; preds = %6, %2
  %10 = phi i1 [ false, %2 ], [ %8, %6 ], !dbg !107
  ret i1 %10, !dbg !107
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1) #0 !dbg !115 !pallas.exprWrapper !77 {
  %3 = alloca %pallas.fracT, align 8
  %4 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !116, metadata !DIExpression()), !dbg !117
  call void @llvm.dbg.value(metadata i32 %1, metadata !118, metadata !DIExpression()), !dbg !117
  %5 = getelementptr inbounds i32, ptr %0, i64 0, !dbg !119
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !120
  %6 = call i1 @pallas.perm(ptr noundef %5, ptr noundef byval(%pallas.fracT) %3), !dbg !121
  %7 = getelementptr inbounds i32, ptr %0, i64 1, !dbg !122
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %4, i32 noundef 1, i32 noundef 1), !dbg !123
  %8 = call i1 @pallas.perm(ptr noundef %7, ptr noundef byval(%pallas.fracT) %4), !dbg !124
  %9 = call i1 @pallas.sepConj(i1 %6, i1 %8), !dbg !125
  ret i1 %9, !dbg !117
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1) #0 !dbg !126 !pallas.exprWrapper !77 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !127, metadata !DIExpression()), !dbg !128
  call void @llvm.dbg.value(metadata i32 %1, metadata !129, metadata !DIExpression()), !dbg !128
  %3 = getelementptr inbounds i32, ptr %0, i64 0, !dbg !130
  %4 = load i32, ptr %3, align 4, !dbg !130
  %5 = icmp eq i32 %4, 0, !dbg !131
  br i1 %5, label %6, label %10, !dbg !132

6:                                                ; preds = %2
  %7 = getelementptr inbounds i32, ptr %0, i64 1, !dbg !133
  %8 = load i32, ptr %7, align 4, !dbg !133
  %9 = icmp eq i32 %8, 0, !dbg !134
  br label %10

10:                                               ; preds = %6, %2
  %11 = phi i1 [ false, %2 ], [ %9, %6 ], !dbg !128
  ret i1 %11, !dbg !128
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0, i32 noundef %1) #0 !dbg !135 !pallas.exprWrapper !77 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !136, metadata !DIExpression()), !dbg !137
  call void @llvm.dbg.value(metadata i32 %1, metadata !138, metadata !DIExpression()), !dbg !137
  %3 = icmp ne ptr %0, null, !dbg !139
  br i1 %3, label %4, label %6, !dbg !140

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 2, !dbg !141
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !137
  ret i1 %7, !dbg !137
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0, i32 noundef %1) #0 !dbg !142 !pallas.exprWrapper !77 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !143, metadata !DIExpression()), !dbg !144
  call void @llvm.dbg.value(metadata i32 %1, metadata !145, metadata !DIExpression()), !dbg !144
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !146
  %4 = sext i32 %1 to i64, !dbg !147
  %5 = icmp eq i64 %3, %4, !dbg !148
  ret i1 %5, !dbg !144
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0, i32 noundef %1) #0 !dbg !149 !pallas.exprWrapper !77 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !150, metadata !DIExpression()), !dbg !151
  call void @llvm.dbg.value(metadata i32 %1, metadata !152, metadata !DIExpression()), !dbg !151
  %4 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !153
  %5 = icmp sle i32 0, %4, !dbg !154
  %6 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !155
  %7 = icmp slt i32 %6, %1, !dbg !156
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !157
  %9 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !158
  %10 = sext i32 %9 to i64, !dbg !159
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !159
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !160
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !161
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !162
  ret i1 %13, !dbg !151
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0, i32 noundef %1) #0 !dbg !163 !pallas.exprWrapper !77 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !164, metadata !DIExpression()), !dbg !165
  call void @llvm.dbg.value(metadata i32 %1, metadata !166, metadata !DIExpression()), !dbg !165
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !167
  %4 = sext i32 %1 to i64, !dbg !168
  %5 = icmp eq i64 %3, %4, !dbg !169
  ret i1 %5, !dbg !165
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0, i32 noundef %1) #0 !dbg !170 !pallas.exprWrapper !77 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !171, metadata !DIExpression()), !dbg !172
  call void @llvm.dbg.value(metadata i32 %1, metadata !173, metadata !DIExpression()), !dbg !172
  %4 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !174
  %5 = icmp sle i32 0, %4, !dbg !175
  %6 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !176
  %7 = icmp slt i32 %6, %1, !dbg !177
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !178
  %9 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !179
  %10 = sext i32 %9 to i64, !dbg !180
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !180
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !181
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !182
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !183
  ret i1 %13, !dbg !172
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0, i32 noundef %1) #0 !dbg !184 !pallas.exprWrapper !77 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !185, metadata !DIExpression()), !dbg !186
  call void @llvm.dbg.value(metadata i32 %1, metadata !187, metadata !DIExpression()), !dbg !186
  %3 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !188
  %4 = icmp sle i32 0, %3, !dbg !189
  %5 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !190
  %6 = icmp slt i32 %5, %1, !dbg !191
  %7 = call i1 @pallas.scAnd(i1 %4, i1 %6), !dbg !192
  %8 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !193
  %9 = sext i32 %8 to i64, !dbg !194
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !194
  %11 = load i32, ptr %10, align 4, !dbg !194
  %12 = icmp eq i32 %11, 0, !dbg !195
  %13 = call i1 @pallas.exists(i1 %7, i1 %12), !dbg !196
  ret i1 %13, !dbg !186
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !197 i64 @pallas.ptrBlockOffset(ptr noundef)

declare !pallas.specLib !198 i64 @pallas.ptrBlockLength(ptr noundef)

declare !pallas.specLib !199 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !200 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !201 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !202 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !203 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !204 i1 @pallas.exists(i1, i1)

declare !pallas.specLib !205 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !206 i32 @pallas.boundVar.0(ptr)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!7, !9}
!llvm.module.flags = !{!14, !15, !16, !17, !18, !19, !20}
!llvm.ident = !{!21, !21}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 89, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "./tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "6c85f6ff57fd2a3ee879ba8b1a4b81a1")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = distinct !DICompileUnit(language: DW_LANG_C11, file: !8, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!8 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_quantifier.c", directory: ".", checksumkind: CSK_MD5, checksum: "96e8f4e260ad1ff84ea13f73e73d9c0d")
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !10, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !11, globals: !13, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "6c85f6ff57fd2a3ee879ba8b1a4b81a1")
!11 = !{!12}
!12 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!13 = !{!0}
!14 = !{i32 7, !"Dwarf Version", i32 5}
!15 = !{i32 2, !"Debug Info Version", i32 3}
!16 = !{i32 1, !"wchar_size", i32 4}
!17 = !{i32 8, !"PIC Level", i32 2}
!18 = !{i32 7, !"PIE Level", i32 2}
!19 = !{i32 7, !"uwtable", i32 2}
!20 = !{i32 7, !"frame-pointer", i32 2}
!21 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!22 = distinct !DISubprogram(name: "bar", scope: !8, file: !8, line: 17, type: !23, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!23 = !DISubroutineType(types: !24)
!24 = !{null, !25, !26}
!25 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !26, size: 64)
!26 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!27 = !{}
!28 = !{!29, i1 false, !30, !34, !36, !38, !40, !42}
!29 = !{!"pallas.srcLoc", i64 7, i64 1, i64 16, i64 1}
!30 = !{!"pallas.requires", !31, ptr @PALLAS_SPEC_0, !32, !33}
!31 = !{!"pallas.srcLoc", i64 8, i64 1, i64 8, i64 30}
!32 = !DILocalVariable(name: "arr", arg: 1, scope: !22, file: !8, line: 17, type: !25)
!33 = !DILocalVariable(name: "n", arg: 2, scope: !22, file: !8, line: 17, type: !26)
!34 = !{!"pallas.requires", !35, ptr @PALLAS_SPEC_1, !32, !33}
!35 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 66}
!36 = !{!"pallas.requires", !37, ptr @PALLAS_SPEC_2, !32, !33}
!37 = !{!"pallas.srcLoc", i64 10, i64 1, i64 11, i64 40}
!38 = !{!"pallas.ensures", !39, ptr @PALLAS_SPEC_3, !32, !33}
!39 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 65}
!40 = !{!"pallas.ensures", !41, ptr @PALLAS_SPEC_4, !32, !33}
!41 = !{!"pallas.srcLoc", i64 13, i64 1, i64 14, i64 39}
!42 = !{!"pallas.ensures", !43, ptr @PALLAS_SPEC_5, !32, !33}
!43 = !{!"pallas.srcLoc", i64 15, i64 1, i64 15, i64 35}
!44 = !DILocation(line: 17, column: 15, scope: !22)
!45 = !DILocation(line: 17, column: 24, scope: !22)
!46 = !DILocation(line: 18, column: 5, scope: !22)
!47 = !DILocation(line: 18, column: 12, scope: !22)
!48 = !DILocation(line: 19, column: 5, scope: !22)
!49 = !DILocation(line: 19, column: 12, scope: !22)
!50 = !DILocation(line: 20, column: 1, scope: !22)
!51 = distinct !DISubprogram(name: "foo", scope: !8, file: !8, line: 34, type: !23, scopeLine: 34, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!52 = !{!53, i1 false, !54, !58, !60, !62, !64, !66}
!53 = !{!"pallas.srcLoc", i64 23, i64 1, i64 33, i64 1}
!54 = !{!"pallas.requires", !55, ptr @PALLAS_SPEC_6, !56, !57}
!55 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 30}
!56 = !DILocalVariable(name: "arr", arg: 1, scope: !51, file: !8, line: 34, type: !25)
!57 = !DILocalVariable(name: "n", arg: 2, scope: !51, file: !8, line: 34, type: !26)
!58 = !{!"pallas.requires", !59, ptr @PALLAS_SPEC_7, !56, !57}
!59 = !{!"pallas.srcLoc", i64 25, i64 1, i64 25, i64 30}
!60 = !{!"pallas.requires", !61, ptr @PALLAS_SPEC_8, !56, !57}
!61 = !{!"pallas.srcLoc", i64 26, i64 1, i64 27, i64 86}
!62 = !{!"pallas.ensures", !63, ptr @PALLAS_SPEC_9, !56, !57}
!63 = !{!"pallas.srcLoc", i64 28, i64 1, i64 28, i64 29}
!64 = !{!"pallas.ensures", !65, ptr @PALLAS_SPEC_10, !56, !57}
!65 = !{!"pallas.srcLoc", i64 29, i64 1, i64 30, i64 85}
!66 = !{!"pallas.ensures", !67, ptr @PALLAS_SPEC_11, !56, !57}
!67 = !{!"pallas.srcLoc", i64 31, i64 1, i64 32, i64 68}
!68 = !DILocation(line: 34, column: 15, scope: !51)
!69 = !DILocation(line: 34, column: 24, scope: !51)
!70 = !DILocation(line: 35, column: 5, scope: !51)
!71 = !DILocation(line: 35, column: 12, scope: !51)
!72 = !DILocation(line: 36, column: 1, scope: !51)
!73 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !8, file: !8, line: 8, type: !74, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!74 = !DISubroutineType(types: !75)
!75 = !{!76, !25, !26}
!76 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!77 = !{!""}
!78 = !DILocalVariable(name: "arr", arg: 1, scope: !73, file: !8, line: 8, type: !25)
!79 = !DILocation(line: 0, scope: !73)
!80 = !DILocalVariable(name: "n", arg: 2, scope: !73, file: !8, line: 8, type: !26)
!81 = !DILocation(line: 8, column: 14, scope: !73)
!82 = !DILocation(line: 8, column: 22, scope: !73)
!83 = !DILocation(line: 8, column: 27, scope: !73)
!84 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !8, file: !8, line: 9, type: !74, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!85 = !DILocalVariable(name: "arr", arg: 1, scope: !84, file: !8, line: 9, type: !25)
!86 = !DILocation(line: 0, scope: !84)
!87 = !DILocalVariable(name: "n", arg: 2, scope: !84, file: !8, line: 9, type: !26)
!88 = !DILocation(line: 9, column: 10, scope: !84)
!89 = !DILocation(line: 9, column: 35, scope: !84)
!90 = !DILocation(line: 9, column: 32, scope: !84)
!91 = !DILocation(line: 9, column: 37, scope: !84)
!92 = !DILocation(line: 9, column: 40, scope: !84)
!93 = !DILocation(line: 9, column: 62, scope: !84)
!94 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !8, file: !8, line: 10, type: !74, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!95 = !DILocalVariable(name: "arr", arg: 1, scope: !94, file: !8, line: 10, type: !25)
!96 = !DILocation(line: 0, scope: !94)
!97 = !DILocalVariable(name: "n", arg: 2, scope: !94, file: !8, line: 10, type: !26)
!98 = !DILocation(line: 10, column: 22, scope: !94)
!99 = !DILocation(line: 10, column: 26, scope: !94)
!100 = !DILocation(line: 10, column: 14, scope: !94)
!101 = !DILocation(line: 11, column: 22, scope: !94)
!102 = !DILocation(line: 11, column: 26, scope: !94)
!103 = !DILocation(line: 11, column: 14, scope: !94)
!104 = !DILocation(line: 10, column: 10, scope: !94)
!105 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !8, file: !8, line: 12, type: !74, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!106 = !DILocalVariable(name: "arr", arg: 1, scope: !105, file: !8, line: 12, type: !25)
!107 = !DILocation(line: 0, scope: !105)
!108 = !DILocalVariable(name: "n", arg: 2, scope: !105, file: !8, line: 12, type: !26)
!109 = !DILocation(line: 12, column: 9, scope: !105)
!110 = !DILocation(line: 12, column: 34, scope: !105)
!111 = !DILocation(line: 12, column: 31, scope: !105)
!112 = !DILocation(line: 12, column: 36, scope: !105)
!113 = !DILocation(line: 12, column: 39, scope: !105)
!114 = !DILocation(line: 12, column: 61, scope: !105)
!115 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !8, file: !8, line: 13, type: !74, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!116 = !DILocalVariable(name: "arr", arg: 1, scope: !115, file: !8, line: 13, type: !25)
!117 = !DILocation(line: 0, scope: !115)
!118 = !DILocalVariable(name: "n", arg: 2, scope: !115, file: !8, line: 13, type: !26)
!119 = !DILocation(line: 13, column: 21, scope: !115)
!120 = !DILocation(line: 13, column: 25, scope: !115)
!121 = !DILocation(line: 13, column: 13, scope: !115)
!122 = !DILocation(line: 14, column: 21, scope: !115)
!123 = !DILocation(line: 14, column: 25, scope: !115)
!124 = !DILocation(line: 14, column: 13, scope: !115)
!125 = !DILocation(line: 13, column: 9, scope: !115)
!126 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !8, file: !8, line: 15, type: !74, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!127 = !DILocalVariable(name: "arr", arg: 1, scope: !126, file: !8, line: 15, type: !25)
!128 = !DILocation(line: 0, scope: !126)
!129 = !DILocalVariable(name: "n", arg: 2, scope: !126, file: !8, line: 15, type: !26)
!130 = !DILocation(line: 15, column: 9, scope: !126)
!131 = !DILocation(line: 15, column: 16, scope: !126)
!132 = !DILocation(line: 15, column: 21, scope: !126)
!133 = !DILocation(line: 15, column: 24, scope: !126)
!134 = !DILocation(line: 15, column: 31, scope: !126)
!135 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !8, file: !8, line: 24, type: !74, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!136 = !DILocalVariable(name: "arr", arg: 1, scope: !135, file: !8, line: 24, type: !25)
!137 = !DILocation(line: 0, scope: !135)
!138 = !DILocalVariable(name: "n", arg: 2, scope: !135, file: !8, line: 24, type: !26)
!139 = !DILocation(line: 24, column: 14, scope: !135)
!140 = !DILocation(line: 24, column: 22, scope: !135)
!141 = !DILocation(line: 24, column: 27, scope: !135)
!142 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !8, file: !8, line: 25, type: !74, scopeLine: 25, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!143 = !DILocalVariable(name: "arr", arg: 1, scope: !142, file: !8, line: 25, type: !25)
!144 = !DILocation(line: 0, scope: !142)
!145 = !DILocalVariable(name: "n", arg: 2, scope: !142, file: !8, line: 25, type: !26)
!146 = !DILocation(line: 25, column: 10, scope: !142)
!147 = !DILocation(line: 25, column: 29, scope: !142)
!148 = !DILocation(line: 25, column: 26, scope: !142)
!149 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !8, file: !8, line: 26, type: !74, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!150 = !DILocalVariable(name: "arr", arg: 1, scope: !149, file: !8, line: 26, type: !25)
!151 = !DILocation(line: 0, scope: !149)
!152 = !DILocalVariable(name: "n", arg: 2, scope: !149, file: !8, line: 26, type: !26)
!153 = !DILocation(line: 26, column: 28, scope: !149)
!154 = !DILocation(line: 26, column: 25, scope: !149)
!155 = !DILocation(line: 27, column: 28, scope: !149)
!156 = !DILocation(line: 27, column: 41, scope: !149)
!157 = !DILocation(line: 26, column: 18, scope: !149)
!158 = !DILocation(line: 27, column: 57, scope: !149)
!159 = !DILocation(line: 27, column: 53, scope: !149)
!160 = !DILocation(line: 27, column: 72, scope: !149)
!161 = !DILocation(line: 27, column: 47, scope: !149)
!162 = !DILocation(line: 26, column: 10, scope: !149)
!163 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !8, file: !8, line: 28, type: !74, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!164 = !DILocalVariable(name: "arr", arg: 1, scope: !163, file: !8, line: 28, type: !25)
!165 = !DILocation(line: 0, scope: !163)
!166 = !DILocalVariable(name: "n", arg: 2, scope: !163, file: !8, line: 28, type: !26)
!167 = !DILocation(line: 28, column: 9, scope: !163)
!168 = !DILocation(line: 28, column: 28, scope: !163)
!169 = !DILocation(line: 28, column: 25, scope: !163)
!170 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !8, file: !8, line: 29, type: !74, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!171 = !DILocalVariable(name: "arr", arg: 1, scope: !170, file: !8, line: 29, type: !25)
!172 = !DILocation(line: 0, scope: !170)
!173 = !DILocalVariable(name: "n", arg: 2, scope: !170, file: !8, line: 29, type: !26)
!174 = !DILocation(line: 29, column: 27, scope: !170)
!175 = !DILocation(line: 29, column: 24, scope: !170)
!176 = !DILocation(line: 30, column: 27, scope: !170)
!177 = !DILocation(line: 30, column: 40, scope: !170)
!178 = !DILocation(line: 29, column: 17, scope: !170)
!179 = !DILocation(line: 30, column: 56, scope: !170)
!180 = !DILocation(line: 30, column: 52, scope: !170)
!181 = !DILocation(line: 30, column: 71, scope: !170)
!182 = !DILocation(line: 30, column: 46, scope: !170)
!183 = !DILocation(line: 29, column: 9, scope: !170)
!184 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !8, file: !8, line: 31, type: !74, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!185 = !DILocalVariable(name: "arr", arg: 1, scope: !184, file: !8, line: 31, type: !25)
!186 = !DILocation(line: 0, scope: !184)
!187 = !DILocalVariable(name: "n", arg: 2, scope: !184, file: !8, line: 31, type: !26)
!188 = !DILocation(line: 31, column: 26, scope: !184)
!189 = !DILocation(line: 31, column: 23, scope: !184)
!190 = !DILocation(line: 32, column: 26, scope: !184)
!191 = !DILocation(line: 32, column: 39, scope: !184)
!192 = !DILocation(line: 31, column: 16, scope: !184)
!193 = !DILocation(line: 32, column: 49, scope: !184)
!194 = !DILocation(line: 32, column: 45, scope: !184)
!195 = !DILocation(line: 32, column: 63, scope: !184)
!196 = !DILocation(line: 31, column: 9, scope: !184)
!197 = !{!"pallas.ptrBlockOffset"}
!198 = !{!"pallas.ptrBlockLength"}
!199 = !{!"pallas.sepConj"}
!200 = !{!"pallas.ptrLength"}
!201 = !{!"pallas.forallSep"}
!202 = !{!"pallas.perm"}
!203 = !{!"pallas.fracOf"}
!204 = !{!"pallas.exists"}
!205 = !{!"pallas.scAnd"}
!206 = !{!"pallas.boundVar"}
