; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_old_fail.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @foo(ptr noundef %0) #0 !dbg !14 !pallas.fcontract !20 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !27, metadata !DIExpression()), !dbg !45
  %3 = load ptr, ptr %2, align 8, !dbg !46
  %4 = load i32, ptr %3, align 4, !dbg !47
  %5 = add nsw i32 %4, 1, !dbg !48
  %6 = load ptr, ptr %2, align 8, !dbg !49
  store i32 %5, ptr %6, align 4, !dbg !50
  ret void, !dbg !51
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !29 !pallas.exprWrapper !52 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !28, metadata !DIExpression()), !dbg !53
  %3 = icmp ne ptr %0, null, !dbg !54
  br i1 %3, label %4, label %6, !dbg !55

4:                                                ; preds = %1
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !56
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !57
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i1 [ false, %1 ], [ %5, %4 ], !dbg !53
  ret i1 %7, !dbg !53
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !38 !pallas.exprWrapper !52 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !37, metadata !DIExpression()), !dbg !58
  %3 = icmp ne ptr %0, null, !dbg !59
  br i1 %3, label %4, label %6, !dbg !60

4:                                                ; preds = %1
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !61
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !62
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i1 [ false, %1 ], [ %5, %4 ], !dbg !58
  ret i1 %7, !dbg !58
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !44 !pallas.exprWrapper !52 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !43, metadata !DIExpression()), !dbg !63
  %2 = load i32, ptr %0, align 4, !dbg !64
  %3 = load i32, ptr %0, align 4, !dbg !65
  %4 = add nsw i32 %3, 2, !dbg !66
  %5 = call i32 @"pallas.old i32_noundef i32"(i32 noundef %4), !dbg !67
  %6 = icmp eq i32 %2, %5, !dbg !68
  ret i1 %6, !dbg !63
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !69 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !70 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !71 i32 @"pallas.old i32_noundef i32"(i32 noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}
!llvm.ident = !{!13, !13}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_old_fail.c", directory: ".", checksumkind: CSK_MD5, checksum: "b04e79a5ef02564cc0d0a97292ad686f")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "bd912f3af77aeb7ebec07336ff7812d0")
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
!14 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 13, type: !15, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!15 = !DISubroutineType(types: !16)
!16 = !{null, !17}
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64)
!18 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!19 = !{}
!20 = !{!21, i1 false, i1 false, !19, !19, !23, !33, !39}
!21 = !{!"pallas.srcLoc", i64 8, i64 1, i64 12, i64 1, !22}
!22 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_old_fail.c", directory: "", checksumkind: CSK_MD5, checksum: "b04e79a5ef02564cc0d0a97292ad686f")
!23 = !{!"pallas.requires", !24, ptr @PALLAS_SPEC_0, !19, !19, !25}
!24 = !{!"pallas.srcLoc", i64 9, i64 1, i64 9, i64 52, !22}
!25 = !{!26}
!26 = !{!27, !28}
!27 = !DILocalVariable(name: "iPtr", arg: 1, scope: !14, file: !1, line: 13, type: !17)
!28 = !DILocalVariable(name: "iPtr", arg: 1, scope: !29, file: !1, line: 9, type: !17)
!29 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 9, type: !30, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!30 = !DISubroutineType(types: !31)
!31 = !{!32, !17}
!32 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!33 = !{!"pallas.ensures", !34, ptr @PALLAS_SPEC_1, !19, !19, !35}
!34 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 52, !22}
!35 = !{!36}
!36 = !{!27, !37}
!37 = !DILocalVariable(name: "iPtr", arg: 1, scope: !38, file: !1, line: 10, type: !17)
!38 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 10, type: !30, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!39 = !{!"pallas.ensures", !40, ptr @PALLAS_SPEC_2, !19, !19, !41}
!40 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 38, !22}
!41 = !{!42}
!42 = !{!27, !43}
!43 = !DILocalVariable(name: "iPtr", arg: 1, scope: !44, file: !1, line: 11, type: !17)
!44 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 11, type: !30, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!45 = !DILocation(line: 13, column: 15, scope: !14)
!46 = !DILocation(line: 14, column: 14, scope: !14)
!47 = !DILocation(line: 14, column: 13, scope: !14)
!48 = !DILocation(line: 14, column: 19, scope: !14)
!49 = !DILocation(line: 14, column: 6, scope: !14)
!50 = !DILocation(line: 14, column: 11, scope: !14)
!51 = !DILocation(line: 15, column: 1, scope: !14)
!52 = !{!""}
!53 = !DILocation(line: 0, scope: !29)
!54 = !DILocation(line: 9, column: 15, scope: !29)
!55 = !DILocation(line: 9, column: 23, scope: !29)
!56 = !DILocation(line: 9, column: 38, scope: !29)
!57 = !DILocation(line: 9, column: 26, scope: !29)
!58 = !DILocation(line: 0, scope: !38)
!59 = !DILocation(line: 10, column: 15, scope: !38)
!60 = !DILocation(line: 10, column: 23, scope: !38)
!61 = !DILocation(line: 10, column: 38, scope: !38)
!62 = !DILocation(line: 10, column: 26, scope: !38)
!63 = !DILocation(line: 0, scope: !44)
!64 = !DILocation(line: 11, column: 9, scope: !44)
!65 = !DILocation(line: 11, column: 28, scope: !44)
!66 = !DILocation(line: 11, column: 34, scope: !44)
!67 = !DILocation(line: 11, column: 18, scope: !44)
!68 = !DILocation(line: 11, column: 15, scope: !44)
!69 = !{!"pallas.perm"}
!70 = !{!"pallas.fracOf"}
!71 = !{!"pallas.old"}
