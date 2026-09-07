; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_multiply.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [5 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_3], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @mult(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !24, metadata !DIExpression()), !dbg !49
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !31, metadata !DIExpression()), !dbg !50
  call void @llvm.dbg.declare(metadata ptr %5, metadata !51, metadata !DIExpression()), !dbg !52
  store i32 0, ptr %5, align 4, !dbg !52
  call void @llvm.dbg.declare(metadata ptr %6, metadata !53, metadata !DIExpression()), !dbg !55
  store i32 0, ptr %6, align 4, !dbg !55
  br label %7, !dbg !56

7:                                                ; preds = %15, %2
  %8 = load i32, ptr %6, align 4, !dbg !57
  %9 = load i32, ptr %4, align 4, !dbg !59
  %10 = icmp slt i32 %8, %9, !dbg !60
  br i1 %10, label %11, label %18, !dbg !61

11:                                               ; preds = %7
  %12 = load i32, ptr %3, align 4, !dbg !62
  %13 = load i32, ptr %5, align 4, !dbg !64
  %14 = add nsw i32 %13, %12, !dbg !64
  store i32 %14, ptr %5, align 4, !dbg !64
  br label %15, !dbg !65

15:                                               ; preds = %11
  %16 = load i32, ptr %6, align 4, !dbg !66
  %17 = add nsw i32 %16, 1, !dbg !66
  store i32 %17, ptr %6, align 4, !dbg !66
  br label %7, !dbg !67, !llvm.loop !68

18:                                               ; preds = %7
  %19 = load i32, ptr %5, align 4, !dbg !99
  ret i32 %19, !dbg !100
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !26 !pallas.exprWrapper !101 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !25, metadata !DIExpression()), !dbg !102
  call void @llvm.dbg.value(metadata i32 %1, metadata !32, metadata !DIExpression()), !dbg !102
  %3 = icmp sge i32 %0, 0, !dbg !103
  ret i1 %3, !dbg !102
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !38 !pallas.exprWrapper !101 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !37, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i32 %1, metadata !40, metadata !DIExpression()), !dbg !104
  %3 = icmp sge i32 %1, 0, !dbg !105
  ret i1 %3, !dbg !104
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1) #0 !dbg !46 !pallas.exprWrapper !101 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !45, metadata !DIExpression()), !dbg !106
  call void @llvm.dbg.value(metadata i32 %1, metadata !48, metadata !DIExpression()), !dbg !106
  %3 = call i32 @"pallas.result i32"(), !dbg !107
  %4 = mul nsw i32 %0, %1, !dbg !108
  %5 = icmp eq i32 %3, %4, !dbg !109
  ret i1 %5, !dbg !106
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !92 !pallas.exprWrapper !101 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !91, metadata !DIExpression()), !dbg !110
  call void @llvm.dbg.value(metadata i32 %1, metadata !94, metadata !DIExpression()), !dbg !110
  call void @llvm.dbg.value(metadata i32 %2, metadata !96, metadata !DIExpression()), !dbg !110
  call void @llvm.dbg.value(metadata i32 %3, metadata !98, metadata !DIExpression()), !dbg !110
  %5 = mul nsw i32 %3, %0, !dbg !111
  %6 = icmp eq i32 %2, %5, !dbg !112
  ret i1 %6, !dbg !110
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(i32 noundef %0, i32 noundef %1, i32 noundef %2, i32 noundef %3) #0 !dbg !78 !pallas.exprWrapper !101 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !77, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %1, metadata !82, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %2, metadata !84, metadata !DIExpression()), !dbg !113
  call void @llvm.dbg.value(metadata i32 %3, metadata !86, metadata !DIExpression()), !dbg !113
  %5 = icmp sle i32 0, %3, !dbg !114
  %6 = icmp sle i32 %3, %1, !dbg !115
  %7 = call i1 @pallas.scAnd(i1 %5, i1 %6), !dbg !116
  ret i1 %7, !dbg !113
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !117 i32 @"pallas.result i32"()

declare !pallas.specLib !118 i1 @pallas.scAnd(i1, i1)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_multiply.c", directory: ".", checksumkind: CSK_MD5, checksum: "61f5e88430c114818b97faa59f80d4ba")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "d62e1e7539bb70cbdc67b8e6afa36958")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "mult", scope: !1, file: !1, line: 13, type: !13, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, i1 false, !16, !16, !20, !33, !41}
!18 = !{!"pallas.srcLoc", i64 8, i64 1, i64 12, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_multiply.c", directory: "", checksumkind: CSK_MD5, checksum: "61f5e88430c114818b97faa59f80d4ba")
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !16, !16, !22}
!21 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 16, !19}
!22 = !{!23, !30}
!23 = !{!24, !25}
!24 = !DILocalVariable(name: "n", arg: 1, scope: !12, file: !1, line: 13, type: !15)
!25 = !DILocalVariable(name: "n", arg: 1, scope: !26, file: !1, line: 9, type: !15)
!26 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 9, type: !27, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!27 = !DISubroutineType(types: !28)
!28 = !{!29, !15, !15}
!29 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!30 = !{!31, !32}
!31 = !DILocalVariable(name: "k", arg: 2, scope: !12, file: !1, line: 13, type: !15)
!32 = !DILocalVariable(name: "k", arg: 2, scope: !26, file: !1, line: 9, type: !15)
!33 = !{!"pallas.requires", !34, ptr @PALLAS_SPEC_1, !16, !16, !35}
!34 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 16, !19}
!35 = !{!36, !39}
!36 = !{!24, !37}
!37 = !DILocalVariable(name: "n", arg: 1, scope: !38, file: !1, line: 10, type: !15)
!38 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 10, type: !27, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!39 = !{!31, !40}
!40 = !DILocalVariable(name: "k", arg: 2, scope: !38, file: !1, line: 10, type: !15)
!41 = !{!"pallas.ensures", !42, ptr @PALLAS_SPEC_2, !16, !16, !43}
!42 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 30, !19}
!43 = !{!44, !47}
!44 = !{!24, !45}
!45 = !DILocalVariable(name: "n", arg: 1, scope: !46, file: !1, line: 11, type: !15)
!46 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 11, type: !27, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!47 = !{!31, !48}
!48 = !DILocalVariable(name: "k", arg: 2, scope: !46, file: !1, line: 11, type: !15)
!49 = !DILocation(line: 13, column: 14, scope: !12)
!50 = !DILocation(line: 13, column: 21, scope: !12)
!51 = !DILocalVariable(name: "res", scope: !12, file: !1, line: 14, type: !15)
!52 = !DILocation(line: 14, column: 9, scope: !12)
!53 = !DILocalVariable(name: "i", scope: !54, file: !1, line: 20, type: !15)
!54 = distinct !DILexicalBlock(scope: !12, file: !1, line: 20, column: 5)
!55 = !DILocation(line: 20, column: 14, scope: !54)
!56 = !DILocation(line: 20, column: 10, scope: !54)
!57 = !DILocation(line: 20, column: 21, scope: !58)
!58 = distinct !DILexicalBlock(scope: !54, file: !1, line: 20, column: 5)
!59 = !DILocation(line: 20, column: 25, scope: !58)
!60 = !DILocation(line: 20, column: 23, scope: !58)
!61 = !DILocation(line: 20, column: 5, scope: !54)
!62 = !DILocation(line: 21, column: 16, scope: !63)
!63 = distinct !DILexicalBlock(scope: !58, file: !1, line: 20, column: 33)
!64 = !DILocation(line: 21, column: 13, scope: !63)
!65 = !DILocation(line: 22, column: 5, scope: !63)
!66 = !DILocation(line: 20, column: 29, scope: !58)
!67 = !DILocation(line: 20, column: 5, scope: !58)
!68 = distinct !{!68, !61, !69, !70, !71}
!69 = !DILocation(line: 22, column: 5, scope: !54)
!70 = !{!"llvm.loop.mustprogress"}
!71 = !{!"pallas.loopInvBlock", !72, !73, !87}
!72 = !{!"pallas.srcLoc", i64 16, i64 5, i64 19, i64 5, !19}
!73 = !{!"pallas.loopInv", !74, ptr @PALLAS_SPEC_3, !16, !16, !75}
!74 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 40, !19}
!75 = !{!76, !81, !83, !85}
!76 = !{!24, !77}
!77 = !DILocalVariable(name: "n", arg: 1, scope: !78, file: !1, line: 17, type: !15)
!78 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 17, type: !79, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!79 = !DISubroutineType(types: !80)
!80 = !{!29, !15, !15, !15, !15}
!81 = !{!31, !82}
!82 = !DILocalVariable(name: "k", arg: 2, scope: !78, file: !1, line: 17, type: !15)
!83 = !{!51, !84}
!84 = !DILocalVariable(name: "res", arg: 3, scope: !78, file: !1, line: 17, type: !15)
!85 = !{!53, !86}
!86 = !DILocalVariable(name: "i", arg: 4, scope: !78, file: !1, line: 17, type: !15)
!87 = !{!"pallas.loopInv", !88, ptr @PALLAS_SPEC_4, !16, !16, !89}
!88 = !{!"pallas.srcLoc", i64 18, i64 5, i64 18, i64 32, !19}
!89 = !{!90, !93, !95, !97}
!90 = !{!24, !91}
!91 = !DILocalVariable(name: "n", arg: 1, scope: !92, file: !1, line: 18, type: !15)
!92 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 18, type: !79, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!93 = !{!31, !94}
!94 = !DILocalVariable(name: "k", arg: 2, scope: !92, file: !1, line: 18, type: !15)
!95 = !{!51, !96}
!96 = !DILocalVariable(name: "res", arg: 3, scope: !92, file: !1, line: 18, type: !15)
!97 = !{!53, !98}
!98 = !DILocalVariable(name: "i", arg: 4, scope: !92, file: !1, line: 18, type: !15)
!99 = !DILocation(line: 24, column: 12, scope: !12)
!100 = !DILocation(line: 24, column: 5, scope: !12)
!101 = !{!""}
!102 = !DILocation(line: 0, scope: !26)
!103 = !DILocation(line: 9, column: 12, scope: !26)
!104 = !DILocation(line: 0, scope: !38)
!105 = !DILocation(line: 10, column: 12, scope: !38)
!106 = !DILocation(line: 0, scope: !46)
!107 = !DILocation(line: 11, column: 9, scope: !46)
!108 = !DILocation(line: 11, column: 27, scope: !46)
!109 = !DILocation(line: 11, column: 22, scope: !46)
!110 = !DILocation(line: 0, scope: !92)
!111 = !DILocation(line: 18, column: 29, scope: !92)
!112 = !DILocation(line: 18, column: 24, scope: !92)
!113 = !DILocation(line: 0, scope: !78)
!114 = !DILocation(line: 17, column: 27, scope: !78)
!115 = !DILocation(line: 17, column: 35, scope: !78)
!116 = !DILocation(line: 17, column: 20, scope: !78)
!117 = !{!"pallas.result"}
!118 = !{!"pallas.scAnd"}
