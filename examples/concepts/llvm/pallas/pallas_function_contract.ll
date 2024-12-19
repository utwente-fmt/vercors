; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_function_contract.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !17 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !21, metadata !DIExpression()), !dbg !25
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !22, metadata !DIExpression()), !dbg !26
  %5 = load i32, ptr %3, align 4, !dbg !27
  %6 = load i32, ptr %4, align 4, !dbg !28
  %7 = mul nsw i32 %5, %6, !dbg !29
  %8 = add nsw i32 %7, 1, !dbg !30
  ret i32 %8, !dbg !31
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bar(i32 noundef %0) #0 !dbg !32 !pallas.fcontract !35 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !39, metadata !DIExpression()), !dbg !40
  call void @llvm.dbg.declare(metadata ptr %3, metadata !41, metadata !DIExpression()), !dbg !42
  store i32 1, ptr %3, align 4, !dbg !42
  %4 = load i32, ptr %2, align 4, !dbg !43
  %5 = load i32, ptr %3, align 4, !dbg !44
  %6 = add nsw i32 %5, %4, !dbg !44
  store i32 %6, ptr %3, align 4, !dbg !44
  %7 = load i32, ptr %3, align 4, !dbg !45
  %8 = mul nsw i32 %7, 42, !dbg !45
  store i32 %8, ptr %3, align 4, !dbg !45
  %9 = load i32, ptr %3, align 4, !dbg !46
  ret i32 %9, !dbg !47
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !48 !pallas.exprWrapper !52 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.value(metadata i32 %1, metadata !55, metadata !DIExpression()), !dbg !54
  %3 = icmp sge i32 %0, 0, !dbg !56
  br i1 %3, label %4, label %6, !dbg !57

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !58
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !54
  ret i1 %7, !dbg !54
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !59 !pallas.exprWrapper !52 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !60, metadata !DIExpression()), !dbg !61
  call void @llvm.dbg.value(metadata i32 %1, metadata !62, metadata !DIExpression()), !dbg !61
  %3 = icmp sge i32 %0, -1, !dbg !63
  br i1 %3, label %4, label %6, !dbg !64

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, -1, !dbg !65
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !61
  ret i1 %7, !dbg !61
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0) #0 !dbg !66 !pallas.exprWrapper !52 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !69, metadata !DIExpression()), !dbg !70
  %2 = icmp slt i32 %0, 0, !dbg !71
  ret i1 %2, !dbg !70
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_function_contract.c", directory: ".", checksumkind: CSK_MD5, checksum: "eaa158c4f64ea69ddbfd098d72f0c838")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "1d11760277123c84c4c47dae01fa0129")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 7, type: !13, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, !19, !23}
!18 = !{!"pallas.srcLoc", i64 3, i64 1, i64 6, i64 2}
!19 = !{!"pallas.requires", !20, ptr @PALLAS_SPEC_0, !21, !22}
!20 = !{!"pallas.srcLoc", i64 4, i64 2, i64 4, i64 27}
!21 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 7, type: !15)
!22 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !1, line: 7, type: !15)
!23 = !{!"pallas.ensures", !24, ptr @PALLAS_SPEC_1, !21, !22}
!24 = !{!"pallas.srcLoc", i64 5, i64 2, i64 5, i64 27}
!25 = !DILocation(line: 7, column: 14, scope: !12)
!26 = !DILocation(line: 7, column: 21, scope: !12)
!27 = !DILocation(line: 10, column: 12, scope: !12)
!28 = !DILocation(line: 10, column: 16, scope: !12)
!29 = !DILocation(line: 10, column: 14, scope: !12)
!30 = !DILocation(line: 10, column: 18, scope: !12)
!31 = !DILocation(line: 10, column: 5, scope: !12)
!32 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 17, type: !33, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!33 = !DISubroutineType(types: !34)
!34 = !{!15, !15}
!35 = !{!36, i1 false, !37}
!36 = !{!"pallas.srcLoc", i64 14, i64 1, i64 16, i64 1}
!37 = !{!"pallas.requires", !38, ptr @PALLAS_SPEC_2, !39}
!38 = !{!"pallas.srcLoc", i64 15, i64 2, i64 15, i64 16}
!39 = !DILocalVariable(name: "x", arg: 1, scope: !32, file: !1, line: 17, type: !15)
!40 = !DILocation(line: 17, column: 14, scope: !32)
!41 = !DILocalVariable(name: "y", scope: !32, file: !1, line: 18, type: !15)
!42 = !DILocation(line: 18, column: 9, scope: !32)
!43 = !DILocation(line: 19, column: 10, scope: !32)
!44 = !DILocation(line: 19, column: 7, scope: !32)
!45 = !DILocation(line: 20, column: 7, scope: !32)
!46 = !DILocation(line: 21, column: 12, scope: !32)
!47 = !DILocation(line: 21, column: 5, scope: !32)
!48 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 4, type: !49, scopeLine: 4, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!49 = !DISubroutineType(types: !50)
!50 = !{!51, !15, !15}
!51 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!52 = !{!""}
!53 = !DILocalVariable(name: "a", arg: 1, scope: !48, file: !1, line: 4, type: !15)
!54 = !DILocation(line: 0, scope: !48)
!55 = !DILocalVariable(name: "b", arg: 2, scope: !48, file: !1, line: 4, type: !15)
!56 = !DILocation(line: 4, column: 13, scope: !48)
!57 = !DILocation(line: 4, column: 18, scope: !48)
!58 = !DILocation(line: 4, column: 23, scope: !48)
!59 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 5, type: !49, scopeLine: 5, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!60 = !DILocalVariable(name: "a", arg: 1, scope: !59, file: !1, line: 5, type: !15)
!61 = !DILocation(line: 0, scope: !59)
!62 = !DILocalVariable(name: "b", arg: 2, scope: !59, file: !1, line: 5, type: !15)
!63 = !DILocation(line: 5, column: 12, scope: !59)
!64 = !DILocation(line: 5, column: 18, scope: !59)
!65 = !DILocation(line: 5, column: 23, scope: !59)
!66 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 15, type: !67, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!67 = !DISubroutineType(types: !68)
!68 = !{!51, !15}
!69 = !DILocalVariable(name: "x", arg: 1, scope: !66, file: !1, line: 15, type: !15)
!70 = !DILocation(line: 0, scope: !66)
!71 = !DILocation(line: 15, column: 13, scope: !66)
