; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_quantifier_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_4], section "llvm.metadata"
@.str = private unnamed_addr constant [2 x i8] c"i\00", align 1, !dbg !0

; Function Attrs: noinline nounwind uwtable
define dso_local void @foo(ptr noundef %0, i32 noundef %1) #0 !dbg !22 !pallas.fcontract !28 {
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
  store i32 2, ptr %8, align 4, !dbg !49
  ret void, !dbg !50
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0, i32 noundef %1) #0 !dbg !51 !pallas.exprWrapper !55 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !56, metadata !DIExpression()), !dbg !57
  call void @llvm.dbg.value(metadata i32 %1, metadata !58, metadata !DIExpression()), !dbg !57
  %4 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !59
  %5 = icmp sle i32 0, %4, !dbg !60
  %6 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !61
  %7 = icmp slt i32 %6, %1, !dbg !62
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !63
  %9 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !64
  %10 = sext i32 %9 to i64, !dbg !65
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !65
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !66
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !67
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !68
  ret i1 %13, !dbg !57
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0, i32 noundef %1) #0 !dbg !69 !pallas.exprWrapper !55 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !70, metadata !DIExpression()), !dbg !71
  call void @llvm.dbg.value(metadata i32 %1, metadata !72, metadata !DIExpression()), !dbg !71
  %3 = icmp ne ptr %0, null, !dbg !73
  br i1 %3, label %4, label %6, !dbg !74

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, 2, !dbg !75
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !71
  ret i1 %7, !dbg !71
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0, i32 noundef %1) #0 !dbg !76 !pallas.exprWrapper !55 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !77, metadata !DIExpression()), !dbg !78
  call void @llvm.dbg.value(metadata i32 %1, metadata !79, metadata !DIExpression()), !dbg !78
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !80
  %4 = sext i32 %1 to i64, !dbg !81
  %5 = icmp eq i64 %3, %4, !dbg !82
  ret i1 %5, !dbg !78
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0, i32 noundef %1) #0 !dbg !83 !pallas.exprWrapper !55 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !84, metadata !DIExpression()), !dbg !85
  call void @llvm.dbg.value(metadata i32 %1, metadata !86, metadata !DIExpression()), !dbg !85
  %3 = call i64 @pallas.ptrLength(ptr noundef %0), !dbg !87
  %4 = sext i32 %1 to i64, !dbg !88
  %5 = icmp eq i64 %3, %4, !dbg !89
  ret i1 %5, !dbg !85
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0, i32 noundef %1) #0 !dbg !90 !pallas.exprWrapper !55 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !91, metadata !DIExpression()), !dbg !92
  call void @llvm.dbg.value(metadata i32 %1, metadata !93, metadata !DIExpression()), !dbg !92
  %3 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !94
  %4 = icmp sle i32 0, %3, !dbg !95
  %5 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !96
  %6 = icmp slt i32 %5, %1, !dbg !97
  %7 = call i1 @pallas.scAnd(i1 %4, i1 %6), !dbg !98
  %8 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !99
  %9 = sext i32 %8 to i64, !dbg !100
  %10 = getelementptr inbounds i32, ptr %0, i64 %9, !dbg !100
  %11 = load i32, ptr %10, align 4, !dbg !100
  %12 = icmp eq i32 %11, 1, !dbg !101
  %13 = call i1 @pallas.exists(i1 %7, i1 %12), !dbg !102
  ret i1 %13, !dbg !92
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0, i32 noundef %1) #0 !dbg !103 !pallas.exprWrapper !55 {
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !104, metadata !DIExpression()), !dbg !105
  call void @llvm.dbg.value(metadata i32 %1, metadata !106, metadata !DIExpression()), !dbg !105
  %4 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !107
  %5 = icmp sle i32 0, %4, !dbg !108
  %6 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !109
  %7 = icmp slt i32 %6, %1, !dbg !110
  %8 = call i1 @pallas.scAnd(i1 %5, i1 %7), !dbg !111
  %9 = call i32 @pallas.boundVar.0(ptr @.str), !dbg !112
  %10 = sext i32 %9 to i64, !dbg !113
  %11 = getelementptr inbounds i32, ptr %0, i64 %10, !dbg !113
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !114
  %12 = call i1 @pallas.perm(ptr noundef %11, ptr noundef byval(%pallas.fracT) %3), !dbg !115
  %13 = call i1 @pallas.forallSep(i1 %8, i1 %12), !dbg !116
  ret i1 %13, !dbg !105
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !117 i64 @pallas.ptrLength(ptr noundef)

declare !pallas.specLib !118 i1 @pallas.exists(i1, i1)

declare !pallas.specLib !119 i1 @pallas.forallSep(i1, i1)

declare !pallas.specLib !120 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !121 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !122 i1 @pallas.scAnd(i1, i1)

declare !pallas.specLib !123 i32 @pallas.boundVar.0(ptr)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!7, !9}
!llvm.module.flags = !{!14, !15, !16, !17, !18, !19, !20}
!llvm.ident = !{!21, !21}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 11, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "./tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "7dcb6f488d321420239a1947cea52c29")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = distinct !DICompileUnit(language: DW_LANG_C11, file: !8, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!8 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_quantifier_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "db1ddd4ee8e816dca9acceb0f763aea2")
!9 = distinct !DICompileUnit(language: DW_LANG_C11, file: !10, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !11, globals: !13, splitDebugInlining: false, nameTableKind: None)
!10 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "7dcb6f488d321420239a1947cea52c29")
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
!22 = distinct !DISubprogram(name: "foo", scope: !8, file: !8, line: 16, type: !23, scopeLine: 16, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!23 = !DISubroutineType(types: !24)
!24 = !{null, !25, !26}
!25 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !26, size: 64)
!26 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!27 = !{}
!28 = !{!29, i1 false, !30, !34, !36, !38, !40, !42}
!29 = !{!"pallas.srcLoc", i64 5, i64 1, i64 15, i64 1}
!30 = !{!"pallas.requires", !31, ptr @PALLAS_SPEC_0, !32, !33}
!31 = !{!"pallas.srcLoc", i64 6, i64 1, i64 6, i64 30}
!32 = !DILocalVariable(name: "arr", arg: 1, scope: !22, file: !8, line: 16, type: !25)
!33 = !DILocalVariable(name: "n", arg: 2, scope: !22, file: !8, line: 16, type: !26)
!34 = !{!"pallas.requires", !35, ptr @PALLAS_SPEC_1, !32, !33}
!35 = !{!"pallas.srcLoc", i64 7, i64 1, i64 7, i64 30}
!36 = !{!"pallas.requires", !37, ptr @PALLAS_SPEC_2, !32, !33}
!37 = !{!"pallas.srcLoc", i64 8, i64 1, i64 9, i64 86}
!38 = !{!"pallas.ensures", !39, ptr @PALLAS_SPEC_3, !32, !33}
!39 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 29}
!40 = !{!"pallas.ensures", !41, ptr @PALLAS_SPEC_4, !32, !33}
!41 = !{!"pallas.srcLoc", i64 11, i64 1, i64 12, i64 85}
!42 = !{!"pallas.ensures", !43, ptr @PALLAS_SPEC_5, !32, !33}
!43 = !{!"pallas.srcLoc", i64 13, i64 1, i64 14, i64 68}
!44 = !DILocation(line: 16, column: 15, scope: !22)
!45 = !DILocation(line: 16, column: 24, scope: !22)
!46 = !DILocation(line: 17, column: 5, scope: !22)
!47 = !DILocation(line: 17, column: 12, scope: !22)
!48 = !DILocation(line: 18, column: 5, scope: !22)
!49 = !DILocation(line: 18, column: 12, scope: !22)
!50 = !DILocation(line: 19, column: 1, scope: !22)
!51 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !8, file: !8, line: 8, type: !52, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!52 = !DISubroutineType(types: !53)
!53 = !{!54, !25, !26}
!54 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!55 = !{!""}
!56 = !DILocalVariable(name: "arr", arg: 1, scope: !51, file: !8, line: 8, type: !25)
!57 = !DILocation(line: 0, scope: !51)
!58 = !DILocalVariable(name: "n", arg: 2, scope: !51, file: !8, line: 8, type: !26)
!59 = !DILocation(line: 8, column: 28, scope: !51)
!60 = !DILocation(line: 8, column: 25, scope: !51)
!61 = !DILocation(line: 9, column: 28, scope: !51)
!62 = !DILocation(line: 9, column: 41, scope: !51)
!63 = !DILocation(line: 8, column: 18, scope: !51)
!64 = !DILocation(line: 9, column: 57, scope: !51)
!65 = !DILocation(line: 9, column: 53, scope: !51)
!66 = !DILocation(line: 9, column: 72, scope: !51)
!67 = !DILocation(line: 9, column: 47, scope: !51)
!68 = !DILocation(line: 8, column: 10, scope: !51)
!69 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !8, file: !8, line: 6, type: !52, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!70 = !DILocalVariable(name: "arr", arg: 1, scope: !69, file: !8, line: 6, type: !25)
!71 = !DILocation(line: 0, scope: !69)
!72 = !DILocalVariable(name: "n", arg: 2, scope: !69, file: !8, line: 6, type: !26)
!73 = !DILocation(line: 6, column: 14, scope: !69)
!74 = !DILocation(line: 6, column: 22, scope: !69)
!75 = !DILocation(line: 6, column: 27, scope: !69)
!76 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !8, file: !8, line: 10, type: !52, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!77 = !DILocalVariable(name: "arr", arg: 1, scope: !76, file: !8, line: 10, type: !25)
!78 = !DILocation(line: 0, scope: !76)
!79 = !DILocalVariable(name: "n", arg: 2, scope: !76, file: !8, line: 10, type: !26)
!80 = !DILocation(line: 10, column: 9, scope: !76)
!81 = !DILocation(line: 10, column: 28, scope: !76)
!82 = !DILocation(line: 10, column: 25, scope: !76)
!83 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !8, file: !8, line: 7, type: !52, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!84 = !DILocalVariable(name: "arr", arg: 1, scope: !83, file: !8, line: 7, type: !25)
!85 = !DILocation(line: 0, scope: !83)
!86 = !DILocalVariable(name: "n", arg: 2, scope: !83, file: !8, line: 7, type: !26)
!87 = !DILocation(line: 7, column: 10, scope: !83)
!88 = !DILocation(line: 7, column: 29, scope: !83)
!89 = !DILocation(line: 7, column: 26, scope: !83)
!90 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !8, file: !8, line: 13, type: !52, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!91 = !DILocalVariable(name: "arr", arg: 1, scope: !90, file: !8, line: 13, type: !25)
!92 = !DILocation(line: 0, scope: !90)
!93 = !DILocalVariable(name: "n", arg: 2, scope: !90, file: !8, line: 13, type: !26)
!94 = !DILocation(line: 13, column: 26, scope: !90)
!95 = !DILocation(line: 13, column: 23, scope: !90)
!96 = !DILocation(line: 14, column: 26, scope: !90)
!97 = !DILocation(line: 14, column: 39, scope: !90)
!98 = !DILocation(line: 13, column: 16, scope: !90)
!99 = !DILocation(line: 14, column: 49, scope: !90)
!100 = !DILocation(line: 14, column: 45, scope: !90)
!101 = !DILocation(line: 14, column: 63, scope: !90)
!102 = !DILocation(line: 13, column: 9, scope: !90)
!103 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !8, file: !8, line: 11, type: !52, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !27)
!104 = !DILocalVariable(name: "arr", arg: 1, scope: !103, file: !8, line: 11, type: !25)
!105 = !DILocation(line: 0, scope: !103)
!106 = !DILocalVariable(name: "n", arg: 2, scope: !103, file: !8, line: 11, type: !26)
!107 = !DILocation(line: 11, column: 27, scope: !103)
!108 = !DILocation(line: 11, column: 24, scope: !103)
!109 = !DILocation(line: 12, column: 27, scope: !103)
!110 = !DILocation(line: 12, column: 40, scope: !103)
!111 = !DILocation(line: 11, column: 17, scope: !103)
!112 = !DILocation(line: 12, column: 56, scope: !103)
!113 = !DILocation(line: 12, column: 52, scope: !103)
!114 = !DILocation(line: 12, column: 71, scope: !103)
!115 = !DILocation(line: 12, column: 46, scope: !103)
!116 = !DILocation(line: 11, column: 9, scope: !103)
!117 = !{!"pallas.ptrLength"}
!118 = !{!"pallas.exists"}
!119 = !{!"pallas.forallSep"}
!120 = !{!"pallas.perm"}
!121 = !{!"pallas.fracOf"}
!122 = !{!"pallas.scAnd"}
!123 = !{!"pallas.boundVar"}
