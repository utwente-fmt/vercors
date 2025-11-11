; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_perm_fail_3.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [2 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(ptr noundef %0) #0 !dbg !14 !pallas.fcontract !20 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !25, metadata !DIExpression()), !dbg !28
  %3 = load ptr, ptr %2, align 8, !dbg !29
  %4 = load i32, ptr %3, align 4, !dbg !30
  %5 = add nsw i32 %4, 1, !dbg !31
  ret i32 %5, !dbg !32
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !33 !pallas.exprWrapper !37 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !38, metadata !DIExpression()), !dbg !39
  %3 = icmp ne ptr %0, null, !dbg !40
  br i1 %3, label %4, label %6, !dbg !41

4:                                                ; preds = %1
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !42
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !43
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i1 [ false, %1 ], [ %5, %4 ], !dbg !39
  ret i1 %7, !dbg !39
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !44 !pallas.exprWrapper !37 {
  %2 = alloca %pallas.fracT, align 8
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !45, metadata !DIExpression()), !dbg !46
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 2, i32 noundef 3), !dbg !47
  %4 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !48
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 2, i32 noundef 3), !dbg !49
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %3), !dbg !50
  %6 = call i1 @pallas.sepConj(i1 %4, i1 %5), !dbg !51
  ret i1 %6, !dbg !46
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !52 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !53 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !54 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}
!llvm.ident = !{!13, !13}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_perm_fail_3.c", directory: ".", checksumkind: CSK_MD5, checksum: "f4230a7fb4eaa07f45d248e201af7902")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "acad0770d37554821ef0983f754f3c97")
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
!14 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 6, type: !15, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!15 = !DISubroutineType(types: !16)
!16 = !{!17, !18}
!17 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!18 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !17, size: 64)
!19 = !{}
!20 = !{!21, i1 false, i1 false, !23, !26}
!21 = !{!"pallas.srcLoc", i64 2, i64 1, i64 5, i64 1, !22}
!22 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_perm_fail_3.c", directory: "", checksumkind: CSK_MD5, checksum: "f4230a7fb4eaa07f45d248e201af7902")
!23 = !{!"pallas.requires", !24, ptr @PALLAS_SPEC_0, !25}
!24 = !{!"pallas.srcLoc", i64 3, i64 1, i64 3, i64 52, !22}
!25 = !DILocalVariable(name: "iPtr", arg: 1, scope: !14, file: !1, line: 6, type: !18)
!26 = !{!"pallas.ensures", !27, ptr @PALLAS_SPEC_1, !25}
!27 = !{!"pallas.srcLoc", i64 4, i64 1, i64 4, i64 67, !22}
!28 = !DILocation(line: 6, column: 14, scope: !14)
!29 = !DILocation(line: 7, column: 13, scope: !14)
!30 = !DILocation(line: 7, column: 12, scope: !14)
!31 = !DILocation(line: 7, column: 18, scope: !14)
!32 = !DILocation(line: 7, column: 5, scope: !14)
!33 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 3, type: !34, scopeLine: 3, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!34 = !DISubroutineType(types: !35)
!35 = !{!36, !18}
!36 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!37 = !{!""}
!38 = !DILocalVariable(name: "iPtr", arg: 1, scope: !33, file: !1, line: 3, type: !18)
!39 = !DILocation(line: 0, scope: !33)
!40 = !DILocation(line: 3, column: 15, scope: !33)
!41 = !DILocation(line: 3, column: 23, scope: !33)
!42 = !DILocation(line: 3, column: 38, scope: !33)
!43 = !DILocation(line: 3, column: 26, scope: !33)
!44 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 4, type: !34, scopeLine: 4, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!45 = !DILocalVariable(name: "iPtr", arg: 1, scope: !44, file: !1, line: 4, type: !18)
!46 = !DILocation(line: 0, scope: !44)
!47 = !DILocation(line: 4, column: 26, scope: !44)
!48 = !DILocation(line: 4, column: 14, scope: !44)
!49 = !DILocation(line: 4, column: 53, scope: !44)
!50 = !DILocation(line: 4, column: 41, scope: !44)
!51 = !DILocation(line: 4, column: 9, scope: !44)
!52 = !{!"pallas.sepConj"}
!53 = !{!"pallas.perm"}
!54 = !{!"pallas.fracOf"}
