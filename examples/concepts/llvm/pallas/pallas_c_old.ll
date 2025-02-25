; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_old.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @foo(ptr noundef %0) #0 !dbg !14 !pallas.fcontract !20 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !24, metadata !DIExpression()), !dbg !29
  %3 = load ptr, ptr %2, align 8, !dbg !30
  %4 = load i32, ptr %3, align 4, !dbg !31
  %5 = add nsw i32 %4, 1, !dbg !32
  %6 = load ptr, ptr %2, align 8, !dbg !33
  store i32 %5, ptr %6, align 4, !dbg !34
  ret void, !dbg !35
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !36 !pallas.exprWrapper !40 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !41, metadata !DIExpression()), !dbg !42
  %2 = load i32, ptr %0, align 4, !dbg !43
  %3 = load i32, ptr %0, align 4, !dbg !44
  %4 = add nsw i32 %3, 1, !dbg !45
  %5 = call i32 @pallas.old.0(i32 noundef %4), !dbg !46
  %6 = icmp eq i32 %2, %5, !dbg !47
  ret i1 %6, !dbg !42
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !48 !pallas.exprWrapper !40 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !49, metadata !DIExpression()), !dbg !50
  %3 = icmp ne ptr %0, null, !dbg !51
  br i1 %3, label %4, label %6, !dbg !52

4:                                                ; preds = %1
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !53
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !54
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i1 [ false, %1 ], [ %5, %4 ], !dbg !50
  ret i1 %7, !dbg !50
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !55 !pallas.exprWrapper !40 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !56, metadata !DIExpression()), !dbg !57
  %3 = icmp ne ptr %0, null, !dbg !58
  br i1 %3, label %4, label %6, !dbg !59

4:                                                ; preds = %1
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !60
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !61
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i1 [ false, %1 ], [ %5, %4 ], !dbg !57
  ret i1 %7, !dbg !57
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !62 i32 @pallas.old.0(i32 noundef)

declare !pallas.specLib !63 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !64 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}
!llvm.ident = !{!13, !13}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_old.c", directory: ".", checksumkind: CSK_MD5, checksum: "481a6559f0b0d1b641ea4539d2c9da96")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "209d8e71b892af3a9258abd680aa86cb")
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
!14 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 15, type: !15, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!15 = !DISubroutineType(types: !16)
!16 = !{null, !17}
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64)
!18 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!19 = !{}
!20 = !{!21, i1 false, !22, !25, !27}
!21 = !{!"pallas.srcLoc", i64 10, i64 1, i64 14, i64 1}
!22 = !{!"pallas.requires", !23, ptr @PALLAS_SPEC_0, !24}
!23 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 50}
!24 = !DILocalVariable(name: "iPtr", arg: 1, scope: !14, file: !1, line: 15, type: !17)
!25 = !{!"pallas.ensures", !26, ptr @PALLAS_SPEC_1, !24}
!26 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 50}
!27 = !{!"pallas.ensures", !28, ptr @PALLAS_SPEC_2, !24}
!28 = !{!"pallas.srcLoc", i64 13, i64 1, i64 13, i64 37}
!29 = !DILocation(line: 15, column: 15, scope: !14)
!30 = !DILocation(line: 16, column: 14, scope: !14)
!31 = !DILocation(line: 16, column: 13, scope: !14)
!32 = !DILocation(line: 16, column: 19, scope: !14)
!33 = !DILocation(line: 16, column: 6, scope: !14)
!34 = !DILocation(line: 16, column: 11, scope: !14)
!35 = !DILocation(line: 17, column: 1, scope: !14)
!36 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 13, type: !37, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!37 = !DISubroutineType(types: !38)
!38 = !{!39, !17}
!39 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!40 = !{!""}
!41 = !DILocalVariable(name: "iPtr", arg: 1, scope: !36, file: !1, line: 13, type: !17)
!42 = !DILocation(line: 0, scope: !36)
!43 = !DILocation(line: 13, column: 9, scope: !36)
!44 = !DILocation(line: 13, column: 27, scope: !36)
!45 = !DILocation(line: 13, column: 33, scope: !36)
!46 = !DILocation(line: 13, column: 18, scope: !36)
!47 = !DILocation(line: 13, column: 15, scope: !36)
!48 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 11, type: !37, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!49 = !DILocalVariable(name: "iPtr", arg: 1, scope: !48, file: !1, line: 11, type: !17)
!50 = !DILocation(line: 0, scope: !48)
!51 = !DILocation(line: 11, column: 15, scope: !48)
!52 = !DILocation(line: 11, column: 23, scope: !48)
!53 = !DILocation(line: 11, column: 37, scope: !48)
!54 = !DILocation(line: 11, column: 26, scope: !48)
!55 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 12, type: !37, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!56 = !DILocalVariable(name: "iPtr", arg: 1, scope: !55, file: !1, line: 12, type: !17)
!57 = !DILocation(line: 0, scope: !55)
!58 = !DILocation(line: 12, column: 15, scope: !55)
!59 = !DILocation(line: 12, column: 23, scope: !55)
!60 = !DILocation(line: 12, column: 37, scope: !55)
!61 = !DILocation(line: 12, column: 26, scope: !55)
!62 = !{!"pallas.old"}
!63 = !{!"pallas.perm"}
!64 = !{!"pallas.fracOf"}
