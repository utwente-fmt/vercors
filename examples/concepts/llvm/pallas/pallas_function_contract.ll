; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_function_contract.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !18 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !22, metadata !DIExpression()), !dbg !26
  store i32 %1, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !23, metadata !DIExpression()), !dbg !27
  %5 = load i32, ptr %3, align 4, !dbg !28
  %6 = load i32, ptr %4, align 4, !dbg !29
  %7 = mul nsw i32 %5, %6, !dbg !30
  %8 = add nsw i32 %7, 1, !dbg !31
  ret i32 %8, !dbg !32
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bar(i32 noundef %0) #0 !dbg !33 !pallas.fcontract !36 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !40, metadata !DIExpression()), !dbg !41
  call void @llvm.dbg.declare(metadata ptr %3, metadata !42, metadata !DIExpression()), !dbg !43
  store i32 1, ptr %3, align 4, !dbg !43
  %4 = load i32, ptr %2, align 4, !dbg !44
  %5 = load i32, ptr %3, align 4, !dbg !45
  %6 = add nsw i32 %5, %4, !dbg !45
  store i32 %6, ptr %3, align 4, !dbg !45
  %7 = load i32, ptr %3, align 4, !dbg !46
  %8 = mul nsw i32 %7, 42, !dbg !46
  store i32 %8, ptr %3, align 4, !dbg !46
  %9 = load i32, ptr %3, align 4, !dbg !47
  ret i32 %9, !dbg !48
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, i32 noundef %1) #0 !dbg !49 !pallas.exprWrapper !53 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !54, metadata !DIExpression()), !dbg !55
  call void @llvm.dbg.value(metadata i32 %1, metadata !56, metadata !DIExpression()), !dbg !55
  %3 = icmp sge i32 %0, 0, !dbg !57
  br i1 %3, label %4, label %6, !dbg !58

4:                                                ; preds = %2
  %5 = icmp sge i32 %1, 0, !dbg !59
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !55
  ret i1 %7, !dbg !55
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, i32 noundef %1) #0 !dbg !60 !pallas.exprWrapper !53 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !61, metadata !DIExpression()), !dbg !62
  call void @llvm.dbg.value(metadata i32 %1, metadata !63, metadata !DIExpression()), !dbg !62
  %3 = icmp sge i32 %0, -1, !dbg !64
  br i1 %3, label %4, label %6, !dbg !65

4:                                                ; preds = %2
  %5 = icmp sgt i32 %1, -1, !dbg !66
  br label %6

6:                                                ; preds = %4, %2
  %7 = phi i1 [ false, %2 ], [ %5, %4 ], !dbg !62
  ret i1 %7, !dbg !62
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0) #0 !dbg !67 !pallas.exprWrapper !53 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !70, metadata !DIExpression()), !dbg !71
  %2 = icmp slt i32 %0, 0, !dbg !72
  ret i1 %2, !dbg !71
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_function_contract.c", directory: ".", checksumkind: CSK_MD5, checksum: "eaa158c4f64ea69ddbfd098d72f0c838")
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
!12 = distinct !DISubprogram(name: "foo", scope: !13, file: !13, line: 7, type: !14, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!13 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_function_contract.c", directory: "", checksumkind: CSK_MD5, checksum: "eaa158c4f64ea69ddbfd098d72f0c838")
!14 = !DISubroutineType(types: !15)
!15 = !{!16, !16, !16}
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !{}
!18 = !{!19, i1 false, i1 false, !20, !24}
!19 = !{!"pallas.srcLoc", i64 3, i64 1, i64 6, i64 2, !13}
!20 = !{!"pallas.requires", !21, ptr @PALLAS_SPEC_0, !22, !23}
!21 = !{!"pallas.srcLoc", i64 4, i64 2, i64 4, i64 27, !13}
!22 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !13, line: 7, type: !16)
!23 = !DILocalVariable(name: "b", arg: 2, scope: !12, file: !13, line: 7, type: !16)
!24 = !{!"pallas.ensures", !25, ptr @PALLAS_SPEC_1, !22, !23}
!25 = !{!"pallas.srcLoc", i64 5, i64 2, i64 5, i64 27, !13}
!26 = !DILocation(line: 7, column: 14, scope: !12)
!27 = !DILocation(line: 7, column: 21, scope: !12)
!28 = !DILocation(line: 10, column: 12, scope: !12)
!29 = !DILocation(line: 10, column: 16, scope: !12)
!30 = !DILocation(line: 10, column: 14, scope: !12)
!31 = !DILocation(line: 10, column: 18, scope: !12)
!32 = !DILocation(line: 10, column: 5, scope: !12)
!33 = distinct !DISubprogram(name: "bar", scope: !13, file: !13, line: 17, type: !34, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!34 = !DISubroutineType(types: !35)
!35 = !{!16, !16}
!36 = !{!37, i1 false, i1 false, !38}
!37 = !{!"pallas.srcLoc", i64 14, i64 1, i64 16, i64 1, !13}
!38 = !{!"pallas.requires", !39, ptr @PALLAS_SPEC_2, !40}
!39 = !{!"pallas.srcLoc", i64 15, i64 2, i64 15, i64 16, !13}
!40 = !DILocalVariable(name: "x", arg: 1, scope: !33, file: !13, line: 17, type: !16)
!41 = !DILocation(line: 17, column: 14, scope: !33)
!42 = !DILocalVariable(name: "y", scope: !33, file: !13, line: 18, type: !16)
!43 = !DILocation(line: 18, column: 9, scope: !33)
!44 = !DILocation(line: 19, column: 10, scope: !33)
!45 = !DILocation(line: 19, column: 7, scope: !33)
!46 = !DILocation(line: 20, column: 7, scope: !33)
!47 = !DILocation(line: 21, column: 12, scope: !33)
!48 = !DILocation(line: 21, column: 5, scope: !33)
!49 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !13, file: !13, line: 4, type: !50, scopeLine: 4, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!50 = !DISubroutineType(types: !51)
!51 = !{!52, !16, !16}
!52 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!53 = !{!""}
!54 = !DILocalVariable(name: "a", arg: 1, scope: !49, file: !13, line: 4, type: !16)
!55 = !DILocation(line: 0, scope: !49)
!56 = !DILocalVariable(name: "b", arg: 2, scope: !49, file: !13, line: 4, type: !16)
!57 = !DILocation(line: 4, column: 13, scope: !49)
!58 = !DILocation(line: 4, column: 18, scope: !49)
!59 = !DILocation(line: 4, column: 23, scope: !49)
!60 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !13, file: !13, line: 5, type: !50, scopeLine: 5, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!61 = !DILocalVariable(name: "a", arg: 1, scope: !60, file: !13, line: 5, type: !16)
!62 = !DILocation(line: 0, scope: !60)
!63 = !DILocalVariable(name: "b", arg: 2, scope: !60, file: !13, line: 5, type: !16)
!64 = !DILocation(line: 5, column: 12, scope: !60)
!65 = !DILocation(line: 5, column: 18, scope: !60)
!66 = !DILocation(line: 5, column: 23, scope: !60)
!67 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !13, file: !13, line: 15, type: !68, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !17)
!68 = !DISubroutineType(types: !69)
!69 = !{!52, !16}
!70 = !DILocalVariable(name: "x", arg: 1, scope: !67, file: !13, line: 15, type: !16)
!71 = !DILocation(line: 0, scope: !67)
!72 = !DILocation(line: 15, column: 13, scope: !67)
