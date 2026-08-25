; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/ghost/c_byval_ghost.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%pallas.seq.i32 = type { i32, i64, i64, i64 }

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @foo(i32 noundef %0) #0 !dbg !12 !pallas.fcontract !17 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !46, metadata !DIExpression()), !dbg !57
  call void @llvm.dbg.declare(metadata ptr %3, metadata !58, metadata !DIExpression()), !dbg !59
  %4 = load i32, ptr %2, align 4, !dbg !60
  %5 = mul nsw i32 %4, 6, !dbg !61
  %6 = mul nsw i32 %5, 7, !dbg !62
  store i32 %6, ptr %3, align 4, !dbg !59
  %7 = load i32, ptr %3, align 4, !dbg !63, !pallas.stmntBlock !64
  ret i32 %7, !dbg !79
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !28 !pallas.exprWrapper !80 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !47, metadata !DIExpression()), !dbg !81
  call void @llvm.dbg.declare(metadata ptr %1, metadata !27, metadata !DIExpression()), !dbg !81
  %3 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %1), !dbg !82
  %4 = icmp ugt i64 %3, 1, !dbg !83
  ret i1 %4, !dbg !81
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0, ptr noundef byval(%pallas.seq.i32) align 8 %1) #0 !dbg !53 !pallas.exprWrapper !80 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !56, metadata !DIExpression()), !dbg !84
  call void @llvm.dbg.declare(metadata ptr %1, metadata !52, metadata !DIExpression()), !dbg !84
  %3 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %1), !dbg !85
  %4 = icmp ugt i64 %3, 1, !dbg !86
  ret i1 %4, !dbg !84
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(i32 noundef %0, i32 noundef %1, ptr noundef byval(%pallas.seq.i32) align 8 %2) #0 !dbg !71 !pallas.exprWrapper !80 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !76, metadata !DIExpression()), !dbg !87
  call void @llvm.dbg.value(metadata i32 %1, metadata !78, metadata !DIExpression()), !dbg !87
  call void @llvm.dbg.declare(metadata ptr %2, metadata !70, metadata !DIExpression()), !dbg !87
  %4 = call i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32) %2), !dbg !88
  %5 = icmp ugt i64 %4, 0, !dbg !89
  ret i1 %5, !dbg !87
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !90 i64 @"pallas.seq.size i32"(ptr noundef byval(%pallas.seq.i32))

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/ghost/c_byval_ghost.c", directory: ".", checksumkind: CSK_MD5, checksum: "de148ec8e8ce9fd6727fa3b21d5cbd0e")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "e8768fa4b6982f63c1018b4bd13eb2fc")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 15, type: !13, scopeLine: 15, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !15}
!15 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!16 = !{}
!17 = !{!18, i1 false, i1 false, !20, !16, !23, !48}
!18 = !{!"pallas.srcLoc", i64 10, i64 1, i64 14, i64 1, !19}
!19 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/ghost/c_byval_ghost.c", directory: "", checksumkind: CSK_MD5, checksum: "de148ec8e8ce9fd6727fa3b21d5cbd0e")
!20 = !{!21}
!21 = !{!22, !"s"}
!22 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 17, !19}
!23 = !{!"pallas.requires", !24, ptr @PALLAS_SPEC_0, !25, !16, !44}
!24 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 30, !19}
!25 = !{!26}
!26 = !{!21, !27}
!27 = !DILocalVariable(name: "s", arg: 2, scope: !28, file: !1, line: 12, type: !32)
!28 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 12, type: !29, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!29 = !DISubroutineType(types: !30)
!30 = !{!31, !15, !32}
!31 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!32 = !DIDerivedType(tag: DW_TAG_typedef, name: "PALLAS_SEQ___int", file: !3, line: 7, baseType: !33)
!33 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !3, line: 7, size: 242003520, elements: !34)
!34 = !{!35, !36}
!35 = !DIDerivedType(tag: DW_TAG_member, name: "contentType", scope: !33, file: !3, line: 7, baseType: !15, size: 32)
!36 = !DIDerivedType(tag: DW_TAG_member, name: "dummy", scope: !33, file: !3, line: 7, baseType: !37, size: 242003488, offset: 32)
!37 = !DICompositeType(tag: DW_TAG_array_type, baseType: !38, size: 242003488, elements: !42)
!38 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !39, line: 26, baseType: !40)
!39 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!40 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !41, line: 41, baseType: !15)
!41 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!42 = !{!43}
!43 = !DISubrange(count: 7562609)
!44 = !{!45}
!45 = !{!46, !47}
!46 = !DILocalVariable(name: "i", arg: 1, scope: !12, file: !1, line: 15, type: !15)
!47 = !DILocalVariable(name: "i", arg: 1, scope: !28, file: !1, line: 12, type: !15)
!48 = !{!"pallas.ensures", !49, ptr @PALLAS_SPEC_1, !50, !16, !54}
!49 = !{!"pallas.srcLoc", i64 13, i64 1, i64 13, i64 30, !19}
!50 = !{!51}
!51 = !{!21, !52}
!52 = !DILocalVariable(name: "s", arg: 2, scope: !53, file: !1, line: 13, type: !32)
!53 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 13, type: !29, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!54 = !{!55}
!55 = !{!46, !56}
!56 = !DILocalVariable(name: "i", arg: 1, scope: !53, file: !1, line: 13, type: !15)
!57 = !DILocation(line: 15, column: 13, scope: !12)
!58 = !DILocalVariable(name: "tmp", scope: !12, file: !1, line: 16, type: !15)
!59 = !DILocation(line: 16, column: 9, scope: !12)
!60 = !DILocation(line: 16, column: 15, scope: !12)
!61 = !DILocation(line: 16, column: 17, scope: !12)
!62 = !DILocation(line: 16, column: 21, scope: !12)
!63 = !DILocation(line: 20, column: 12, scope: !12)
!64 = !{!65, !66}
!65 = !{!"pallas.srcLoc", i64 17, i64 5, i64 19, i64 5, !19}
!66 = !{!"pallas.assert", !67, ptr @PALLAS_SPEC_2, !68, !16, !74}
!67 = !{!"pallas.srcLoc", i64 18, i64 5, i64 18, i64 32, !19}
!68 = !{!69}
!69 = !{!21, !70}
!70 = !DILocalVariable(name: "s", arg: 3, scope: !71, file: !1, line: 18, type: !32)
!71 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 18, type: !72, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !16)
!72 = !DISubroutineType(types: !73)
!73 = !{!31, !15, !15, !32}
!74 = !{!75, !77}
!75 = !{!46, !76}
!76 = !DILocalVariable(name: "i", arg: 1, scope: !71, file: !1, line: 18, type: !15)
!77 = !{!58, !78}
!78 = !DILocalVariable(name: "tmp", arg: 2, scope: !71, file: !1, line: 18, type: !15)
!79 = !DILocation(line: 20, column: 5, scope: !12)
!80 = !{!""}
!81 = !DILocation(line: 0, scope: !28)
!82 = !DILocation(line: 12, column: 10, scope: !28)
!83 = !DILocation(line: 12, column: 27, scope: !28)
!84 = !DILocation(line: 0, scope: !53)
!85 = !DILocation(line: 13, column: 10, scope: !53)
!86 = !DILocation(line: 13, column: 27, scope: !53)
!87 = !DILocation(line: 0, scope: !71)
!88 = !DILocation(line: 18, column: 12, scope: !71)
!89 = !DILocation(line: 18, column: 29, scope: !71)
!90 = !{!"pallas.seq.size"}
