; ModuleID = 'tmp_spectral/tmp_ir_source0.ll'
source_filename = "examples/publications/2026/ATVA2026Spectral/C/date.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @later(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 !dbg !12 !pallas.fcontract !18 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %1, metadata !29, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %2, metadata !32, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %3, metadata !35, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %4, metadata !38, metadata !DIExpression()), !dbg !123
  call void @llvm.dbg.value(metadata i32 %5, metadata !41, metadata !DIExpression()), !dbg !123
  %7 = icmp ne i32 %0, %3, !dbg !124
  br i1 %7, label %8, label %10, !dbg !126

8:                                                ; preds = %6
  %9 = icmp sgt i32 %0, %3, !dbg !127
  br label %16, !dbg !129

10:                                               ; preds = %6
  %11 = icmp ne i32 %1, %4, !dbg !130
  br i1 %11, label %12, label %14, !dbg !132

12:                                               ; preds = %10
  %13 = icmp sgt i32 %1, %4, !dbg !133
  br label %16, !dbg !135

14:                                               ; preds = %10
  %15 = icmp sgt i32 %2, %5, !dbg !136
  br label %16, !dbg !138

16:                                               ; preds = %14, %12, %8
  %.0 = phi i1 [ %9, %8 ], [ %13, %12 ], [ %15, %14 ], !dbg !139
  ret i1 %.0, !dbg !140
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @test() #0 !dbg !141 {
  %1 = call zeroext i1 @later(i32 noundef 2023, i32 noundef 3, i32 noundef 7, i32 noundef 2023, i32 noundef 1, i32 noundef 1), !dbg !144
  %2 = call zeroext i1 @later(i32 noundef 1, i32 noundef 1, i32 noundef 2023, i32 noundef 15, i32 noundef 3, i32 noundef 2023), !dbg !145
  ret i32 0, !dbg !146
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 !dbg !27 !pallas.exprWrapper !147 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !26, metadata !DIExpression()), !dbg !148
  call void @llvm.dbg.value(metadata i32 %1, metadata !30, metadata !DIExpression()), !dbg !148
  call void @llvm.dbg.value(metadata i32 %2, metadata !33, metadata !DIExpression()), !dbg !148
  call void @llvm.dbg.value(metadata i32 %3, metadata !36, metadata !DIExpression()), !dbg !148
  call void @llvm.dbg.value(metadata i32 %4, metadata !39, metadata !DIExpression()), !dbg !148
  call void @llvm.dbg.value(metadata i32 %5, metadata !42, metadata !DIExpression()), !dbg !148
  %7 = icmp sle i32 1, %1, !dbg !149
  %8 = icmp sle i32 %1, 12, !dbg !150
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !151
  ret i1 %9, !dbg !148
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 !dbg !48 !pallas.exprWrapper !147 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !47, metadata !DIExpression()), !dbg !152
  call void @llvm.dbg.value(metadata i32 %1, metadata !50, metadata !DIExpression()), !dbg !152
  call void @llvm.dbg.value(metadata i32 %2, metadata !52, metadata !DIExpression()), !dbg !152
  call void @llvm.dbg.value(metadata i32 %3, metadata !54, metadata !DIExpression()), !dbg !152
  call void @llvm.dbg.value(metadata i32 %4, metadata !56, metadata !DIExpression()), !dbg !152
  call void @llvm.dbg.value(metadata i32 %5, metadata !58, metadata !DIExpression()), !dbg !152
  %7 = icmp sle i32 1, %2, !dbg !153
  %8 = icmp sle i32 %2, 31, !dbg !154
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !155
  ret i1 %9, !dbg !152
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 !dbg !64 !pallas.exprWrapper !147 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !63, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %1, metadata !66, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %2, metadata !68, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %3, metadata !70, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %4, metadata !72, metadata !DIExpression()), !dbg !156
  call void @llvm.dbg.value(metadata i32 %5, metadata !74, metadata !DIExpression()), !dbg !156
  %7 = icmp sle i32 1, %4, !dbg !157
  %8 = icmp sle i32 %4, 12, !dbg !158
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !159
  ret i1 %9, !dbg !156
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 !dbg !80 !pallas.exprWrapper !147 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !79, metadata !DIExpression()), !dbg !160
  call void @llvm.dbg.value(metadata i32 %1, metadata !82, metadata !DIExpression()), !dbg !160
  call void @llvm.dbg.value(metadata i32 %2, metadata !84, metadata !DIExpression()), !dbg !160
  call void @llvm.dbg.value(metadata i32 %3, metadata !86, metadata !DIExpression()), !dbg !160
  call void @llvm.dbg.value(metadata i32 %4, metadata !88, metadata !DIExpression()), !dbg !160
  call void @llvm.dbg.value(metadata i32 %5, metadata !90, metadata !DIExpression()), !dbg !160
  %7 = icmp sle i32 1, %5, !dbg !161
  %8 = icmp sle i32 %5, 31, !dbg !162
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !163
  ret i1 %9, !dbg !160
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 !dbg !96 !pallas.exprWrapper !147 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !95, metadata !DIExpression()), !dbg !164
  call void @llvm.dbg.value(metadata i32 %1, metadata !98, metadata !DIExpression()), !dbg !164
  call void @llvm.dbg.value(metadata i32 %2, metadata !100, metadata !DIExpression()), !dbg !164
  call void @llvm.dbg.value(metadata i32 %3, metadata !102, metadata !DIExpression()), !dbg !164
  call void @llvm.dbg.value(metadata i32 %4, metadata !104, metadata !DIExpression()), !dbg !164
  call void @llvm.dbg.value(metadata i32 %5, metadata !106, metadata !DIExpression()), !dbg !164
  %7 = icmp sgt i32 %0, %3, !dbg !165
  %8 = call zeroext i1 @"pallas.result zeroext i1"(), !dbg !166
  %9 = zext i1 %8 to i32, !dbg !166
  %10 = icmp eq i32 %9, 1, !dbg !167
  %11 = call i1 @pallas.imply(i1 %7, i1 %10), !dbg !168
  ret i1 %11, !dbg !164
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 !dbg !112 !pallas.exprWrapper !147 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !111, metadata !DIExpression()), !dbg !169
  call void @llvm.dbg.value(metadata i32 %1, metadata !114, metadata !DIExpression()), !dbg !169
  call void @llvm.dbg.value(metadata i32 %2, metadata !116, metadata !DIExpression()), !dbg !169
  call void @llvm.dbg.value(metadata i32 %3, metadata !118, metadata !DIExpression()), !dbg !169
  call void @llvm.dbg.value(metadata i32 %4, metadata !120, metadata !DIExpression()), !dbg !169
  call void @llvm.dbg.value(metadata i32 %5, metadata !122, metadata !DIExpression()), !dbg !169
  %7 = icmp eq i32 %0, %3, !dbg !170
  %8 = icmp eq i32 %1, %4, !dbg !171
  %9 = call i1 @pallas.scAnd(i1 %7, i1 %8), !dbg !172
  %10 = call zeroext i1 @"pallas.result zeroext i1"(), !dbg !173
  %11 = zext i1 %10 to i32, !dbg !173
  %12 = icmp sgt i32 %2, %5, !dbg !174
  %13 = zext i1 %12 to i32, !dbg !174
  %14 = icmp eq i32 %11, %13, !dbg !175
  %15 = call i1 @pallas.imply(i1 %9, i1 %14), !dbg !176
  ret i1 %15, !dbg !169
}

declare !pallas.specLib !177 i1 @pallas.imply(i1, i1)

declare !pallas.specLib !178 zeroext i1 @"pallas.result zeroext i1"()

declare !pallas.specLib !179 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/publications/2026/ATVA2026Spectral/C/date.c", directory: ".", checksumkind: CSK_MD5, checksum: "59b73f088c2bcbd94aa9f886d47ab3e2")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp_spectral/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "8c868f029c22283419fb4ae57afcadfb")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "later", scope: !1, file: !1, line: 18, type: !13, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !16, !16, !16, !16, !16, !16}
!15 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !{}
!18 = !{!19, i1 false, i1 false, !17, !17, !21, !43, !59, !75, !91, !107}
!19 = !{!"pallas.srcLoc", i64 8, i64 1, i64 17, i64 1, !20}
!20 = !DIFile(filename: "/home/rme/repos/vercors/examples/publications/2026/ATVA2026Spectral/C/date.c", directory: "", checksumkind: CSK_MD5, checksum: "59b73f088c2bcbd94aa9f886d47ab3e2")
!21 = !{!"pallas.requires", !22, ptr @PALLAS_SPEC_0, !17, !17, !23}
!22 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 33, !20}
!23 = !{!24, !28, !31, !34, !37, !40}
!24 = !{!25, !26}
!25 = !DILocalVariable(name: "y1", arg: 1, scope: !12, file: !1, line: 18, type: !16)
!26 = !DILocalVariable(name: "y1", arg: 1, scope: !27, file: !1, line: 9, type: !16)
!27 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 9, type: !13, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!28 = !{!29, !30}
!29 = !DILocalVariable(name: "m1", arg: 2, scope: !12, file: !1, line: 18, type: !16)
!30 = !DILocalVariable(name: "m1", arg: 2, scope: !27, file: !1, line: 9, type: !16)
!31 = !{!32, !33}
!32 = !DILocalVariable(name: "d1", arg: 3, scope: !12, file: !1, line: 18, type: !16)
!33 = !DILocalVariable(name: "d1", arg: 3, scope: !27, file: !1, line: 9, type: !16)
!34 = !{!35, !36}
!35 = !DILocalVariable(name: "y2", arg: 4, scope: !12, file: !1, line: 19, type: !16)
!36 = !DILocalVariable(name: "y2", arg: 4, scope: !27, file: !1, line: 9, type: !16)
!37 = !{!38, !39}
!38 = !DILocalVariable(name: "m2", arg: 5, scope: !12, file: !1, line: 19, type: !16)
!39 = !DILocalVariable(name: "m2", arg: 5, scope: !27, file: !1, line: 9, type: !16)
!40 = !{!41, !42}
!41 = !DILocalVariable(name: "d2", arg: 6, scope: !12, file: !1, line: 19, type: !16)
!42 = !DILocalVariable(name: "d2", arg: 6, scope: !27, file: !1, line: 9, type: !16)
!43 = !{!"pallas.requires", !44, ptr @PALLAS_SPEC_1, !17, !17, !45}
!44 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 33, !20}
!45 = !{!46, !49, !51, !53, !55, !57}
!46 = !{!25, !47}
!47 = !DILocalVariable(name: "y1", arg: 1, scope: !48, file: !1, line: 10, type: !16)
!48 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 10, type: !13, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!49 = !{!29, !50}
!50 = !DILocalVariable(name: "m1", arg: 2, scope: !48, file: !1, line: 10, type: !16)
!51 = !{!32, !52}
!52 = !DILocalVariable(name: "d1", arg: 3, scope: !48, file: !1, line: 10, type: !16)
!53 = !{!35, !54}
!54 = !DILocalVariable(name: "y2", arg: 4, scope: !48, file: !1, line: 10, type: !16)
!55 = !{!38, !56}
!56 = !DILocalVariable(name: "m2", arg: 5, scope: !48, file: !1, line: 10, type: !16)
!57 = !{!41, !58}
!58 = !DILocalVariable(name: "d2", arg: 6, scope: !48, file: !1, line: 10, type: !16)
!59 = !{!"pallas.requires", !60, ptr @PALLAS_SPEC_2, !17, !17, !61}
!60 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 33, !20}
!61 = !{!62, !65, !67, !69, !71, !73}
!62 = !{!25, !63}
!63 = !DILocalVariable(name: "y1", arg: 1, scope: !64, file: !1, line: 11, type: !16)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 11, type: !13, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!65 = !{!29, !66}
!66 = !DILocalVariable(name: "m1", arg: 2, scope: !64, file: !1, line: 11, type: !16)
!67 = !{!32, !68}
!68 = !DILocalVariable(name: "d1", arg: 3, scope: !64, file: !1, line: 11, type: !16)
!69 = !{!35, !70}
!70 = !DILocalVariable(name: "y2", arg: 4, scope: !64, file: !1, line: 11, type: !16)
!71 = !{!38, !72}
!72 = !DILocalVariable(name: "m2", arg: 5, scope: !64, file: !1, line: 11, type: !16)
!73 = !{!41, !74}
!74 = !DILocalVariable(name: "d2", arg: 6, scope: !64, file: !1, line: 11, type: !16)
!75 = !{!"pallas.requires", !76, ptr @PALLAS_SPEC_3, !17, !17, !77}
!76 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 33, !20}
!77 = !{!78, !81, !83, !85, !87, !89}
!78 = !{!25, !79}
!79 = !DILocalVariable(name: "y1", arg: 1, scope: !80, file: !1, line: 12, type: !16)
!80 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 12, type: !13, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!81 = !{!29, !82}
!82 = !DILocalVariable(name: "m1", arg: 2, scope: !80, file: !1, line: 12, type: !16)
!83 = !{!32, !84}
!84 = !DILocalVariable(name: "d1", arg: 3, scope: !80, file: !1, line: 12, type: !16)
!85 = !{!35, !86}
!86 = !DILocalVariable(name: "y2", arg: 4, scope: !80, file: !1, line: 12, type: !16)
!87 = !{!38, !88}
!88 = !DILocalVariable(name: "m2", arg: 5, scope: !80, file: !1, line: 12, type: !16)
!89 = !{!41, !90}
!90 = !DILocalVariable(name: "d2", arg: 6, scope: !80, file: !1, line: 12, type: !16)
!91 = !{!"pallas.ensures", !92, ptr @PALLAS_SPEC_4, !17, !17, !93}
!92 = !{!"pallas.srcLoc", i64 13, i64 1, i64 14, i64 38, !20}
!93 = !{!94, !97, !99, !101, !103, !105}
!94 = !{!25, !95}
!95 = !DILocalVariable(name: "y1", arg: 1, scope: !96, file: !1, line: 13, type: !16)
!96 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 13, type: !13, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!97 = !{!29, !98}
!98 = !DILocalVariable(name: "m1", arg: 2, scope: !96, file: !1, line: 13, type: !16)
!99 = !{!32, !100}
!100 = !DILocalVariable(name: "d1", arg: 3, scope: !96, file: !1, line: 13, type: !16)
!101 = !{!35, !102}
!102 = !DILocalVariable(name: "y2", arg: 4, scope: !96, file: !1, line: 13, type: !16)
!103 = !{!38, !104}
!104 = !DILocalVariable(name: "m2", arg: 5, scope: !96, file: !1, line: 13, type: !16)
!105 = !{!41, !106}
!106 = !DILocalVariable(name: "d2", arg: 6, scope: !96, file: !1, line: 13, type: !16)
!107 = !{!"pallas.ensures", !108, ptr @PALLAS_SPEC_5, !17, !17, !109}
!108 = !{!"pallas.srcLoc", i64 15, i64 1, i64 16, i64 42, !20}
!109 = !{!110, !113, !115, !117, !119, !121}
!110 = !{!25, !111}
!111 = !DILocalVariable(name: "y1", arg: 1, scope: !112, file: !1, line: 15, type: !16)
!112 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 15, type: !13, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!113 = !{!29, !114}
!114 = !DILocalVariable(name: "m1", arg: 2, scope: !112, file: !1, line: 15, type: !16)
!115 = !{!32, !116}
!116 = !DILocalVariable(name: "d1", arg: 3, scope: !112, file: !1, line: 15, type: !16)
!117 = !{!35, !118}
!118 = !DILocalVariable(name: "y2", arg: 4, scope: !112, file: !1, line: 15, type: !16)
!119 = !{!38, !120}
!120 = !DILocalVariable(name: "m2", arg: 5, scope: !112, file: !1, line: 15, type: !16)
!121 = !{!41, !122}
!122 = !DILocalVariable(name: "d2", arg: 6, scope: !112, file: !1, line: 15, type: !16)
!123 = !DILocation(line: 0, scope: !12)
!124 = !DILocation(line: 20, column: 12, scope: !125)
!125 = distinct !DILexicalBlock(scope: !12, file: !1, line: 20, column: 9)
!126 = !DILocation(line: 20, column: 9, scope: !12)
!127 = !DILocation(line: 21, column: 19, scope: !128)
!128 = distinct !DILexicalBlock(scope: !125, file: !1, line: 20, column: 19)
!129 = !DILocation(line: 21, column: 9, scope: !128)
!130 = !DILocation(line: 22, column: 19, scope: !131)
!131 = distinct !DILexicalBlock(scope: !125, file: !1, line: 22, column: 16)
!132 = !DILocation(line: 22, column: 16, scope: !125)
!133 = !DILocation(line: 23, column: 19, scope: !134)
!134 = distinct !DILexicalBlock(scope: !131, file: !1, line: 22, column: 26)
!135 = !DILocation(line: 23, column: 9, scope: !134)
!136 = !DILocation(line: 25, column: 19, scope: !137)
!137 = distinct !DILexicalBlock(scope: !131, file: !1, line: 24, column: 12)
!138 = !DILocation(line: 25, column: 9, scope: !137)
!139 = !DILocation(line: 0, scope: !125)
!140 = !DILocation(line: 27, column: 1, scope: !12)
!141 = distinct !DISubprogram(name: "test", scope: !1, file: !1, line: 29, type: !142, scopeLine: 29, spFlags: DISPFlagDefinition, unit: !0)
!142 = !DISubroutineType(types: !143)
!143 = !{!16}
!144 = !DILocation(line: 30, column: 5, scope: !141)
!145 = !DILocation(line: 32, column: 5, scope: !141)
!146 = !DILocation(line: 34, column: 5, scope: !141)
!147 = !{!""}
!148 = !DILocation(line: 0, scope: !27)
!149 = !DILocation(line: 9, column: 17, scope: !27)
!150 = !DILocation(line: 9, column: 27, scope: !27)
!151 = !DILocation(line: 9, column: 10, scope: !27)
!152 = !DILocation(line: 0, scope: !48)
!153 = !DILocation(line: 10, column: 17, scope: !48)
!154 = !DILocation(line: 10, column: 27, scope: !48)
!155 = !DILocation(line: 10, column: 10, scope: !48)
!156 = !DILocation(line: 0, scope: !64)
!157 = !DILocation(line: 11, column: 17, scope: !64)
!158 = !DILocation(line: 11, column: 27, scope: !64)
!159 = !DILocation(line: 11, column: 10, scope: !64)
!160 = !DILocation(line: 0, scope: !80)
!161 = !DILocation(line: 12, column: 17, scope: !80)
!162 = !DILocation(line: 12, column: 27, scope: !80)
!163 = !DILocation(line: 12, column: 10, scope: !80)
!164 = !DILocation(line: 0, scope: !96)
!165 = !DILocation(line: 13, column: 19, scope: !96)
!166 = !DILocation(line: 14, column: 15, scope: !96)
!167 = !DILocation(line: 14, column: 30, scope: !96)
!168 = !DILocation(line: 13, column: 9, scope: !96)
!169 = !DILocation(line: 0, scope: !112)
!170 = !DILocation(line: 15, column: 24, scope: !112)
!171 = !DILocation(line: 15, column: 34, scope: !112)
!172 = !DILocation(line: 15, column: 16, scope: !112)
!173 = !DILocation(line: 16, column: 16, scope: !112)
!174 = !DILocation(line: 16, column: 37, scope: !112)
!175 = !DILocation(line: 16, column: 31, scope: !112)
!176 = !DILocation(line: 15, column: 9, scope: !112)
!177 = !{!"pallas.imply"}
!178 = !{!"pallas.result"}
!179 = !{!"pallas.scAnd"}
