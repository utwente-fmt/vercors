; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_perm_fail_1.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [2 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(ptr noundef %0) #0 !dbg !14 !pallas.fcontract !20 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !27, metadata !DIExpression()), !dbg !39
  %3 = load ptr, ptr %2, align 8, !dbg !40
  %4 = load i32, ptr %3, align 4, !dbg !41
  %5 = add nsw i32 %4, 5, !dbg !42
  ret i32 %5, !dbg !43
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !29 !pallas.exprWrapper !44 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !28, metadata !DIExpression()), !dbg !45
  %3 = icmp ne ptr %0, null, !dbg !46
  br i1 %3, label %4, label %6, !dbg !47

4:                                                ; preds = %1
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 4), !dbg !48
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !49
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i1 [ false, %1 ], [ %5, %4 ], !dbg !45
  ret i1 %7, !dbg !45
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !38 !pallas.exprWrapper !44 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !37, metadata !DIExpression()), !dbg !50
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !51
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !52
  ret i1 %3, !dbg !50
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !53 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !54 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}
!llvm.ident = !{!13, !13}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_perm_fail_1.c", directory: ".", checksumkind: CSK_MD5, checksum: "1e389885e8126e26d04990f20a199125")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "f59bce45437bdda3ec68d236659243f4")
!4 = !{!5}
!5 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!6 = !{i32 7, !"Dwarf Version", i32 5}
!7 = !{i32 2, !"Debug Info Version", i32 3}
!8 = !{i32 1, !"wchar_size", i32 4}
!9 = !{i32 8, !"PIC Level", i32 2}
!10 = !{i32 7, !"PIE Level", i32 2}
!11 = !{i32 7, !"uwtable", i32 2}
!12 = !{i32 7, !"frame-pointer", i32 2}
!13 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!14 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 8, type: !15, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!15 = !DISubroutineType(types: !16)
!16 = !{!17, !18}
!17 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!18 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !17, size: 64)
!19 = !{}
!20 = !{!21, i1 false, i1 false, !19, !19, !23, !33}
!21 = !{!"pallas.srcLoc", i64 4, i64 1, i64 7, i64 1, !22}
!22 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_perm_fail_1.c", directory: "", checksumkind: CSK_MD5, checksum: "1e389885e8126e26d04990f20a199125")
!23 = !{!"pallas.requires", !24, ptr @PALLAS_SPEC_0, !19, !19, !25}
!24 = !{!"pallas.srcLoc", i64 5, i64 1, i64 5, i64 50, !22}
!25 = !{!26}
!26 = !{!27, !28}
!27 = !DILocalVariable(name: "ptr", arg: 1, scope: !14, file: !1, line: 8, type: !18)
!28 = !DILocalVariable(name: "ptr", arg: 1, scope: !29, file: !1, line: 5, type: !18)
!29 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 5, type: !30, scopeLine: 5, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!30 = !DISubroutineType(types: !31)
!31 = !{!32, !18}
!32 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!33 = !{!"pallas.ensures", !34, ptr @PALLAS_SPEC_1, !19, !19, !35}
!34 = !{!"pallas.srcLoc", i64 6, i64 1, i64 6, i64 34, !22}
!35 = !{!36}
!36 = !{!27, !37}
!37 = !DILocalVariable(name: "ptr", arg: 1, scope: !38, file: !1, line: 6, type: !18)
!38 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 6, type: !30, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!39 = !DILocation(line: 8, column: 14, scope: !14)
!40 = !DILocation(line: 9, column: 13, scope: !14)
!41 = !DILocation(line: 9, column: 12, scope: !14)
!42 = !DILocation(line: 9, column: 17, scope: !14)
!43 = !DILocation(line: 9, column: 5, scope: !14)
!44 = !{!""}
!45 = !DILocation(line: 0, scope: !29)
!46 = !DILocation(line: 5, column: 14, scope: !29)
!47 = !DILocation(line: 5, column: 22, scope: !29)
!48 = !DILocation(line: 5, column: 36, scope: !29)
!49 = !DILocation(line: 5, column: 25, scope: !29)
!50 = !DILocation(line: 0, scope: !38)
!51 = !DILocation(line: 6, column: 20, scope: !38)
!52 = !DILocation(line: 6, column: 9, scope: !38)
!53 = !{!"pallas.perm"}
!54 = !{!"pallas.fracOf"}
