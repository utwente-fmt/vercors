; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_result.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.s = type { i64, i64, i64, i64, i64, i64, i64 }

@llvm.used = appending global [2 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @fun(ptr noalias sret(%struct.s) align 8 %0, i32 noundef %1) #0 !dbg !12 !pallas.fcontract !32 {
  %3 = alloca i32, align 4
  store i32 %1, ptr %3, align 4
  call void @llvm.dbg.declare(metadata ptr %3, metadata !39, metadata !DIExpression()), !dbg !45
  call void @llvm.dbg.declare(metadata ptr %0, metadata !46, metadata !DIExpression()), !dbg !47
  %4 = getelementptr inbounds %struct.s, ptr %0, i32 0, i32 0, !dbg !48
  store i64 0, ptr %4, align 8, !dbg !49
  %5 = getelementptr inbounds %struct.s, ptr %0, i32 0, i32 1, !dbg !50
  store i64 1, ptr %5, align 8, !dbg !51
  ret void, !dbg !52
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bar(i32 noundef %0) #0 !dbg !53 !pallas.fcontract !56 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !62, metadata !DIExpression()), !dbg !65
  call void @llvm.dbg.declare(metadata ptr %3, metadata !66, metadata !DIExpression()), !dbg !67
  %4 = load i32, ptr %2, align 4, !dbg !68
  %5 = icmp sgt i32 %4, 0, !dbg !69
  br i1 %5, label %6, label %8, !dbg !68

6:                                                ; preds = %1
  %7 = load i32, ptr %2, align 4, !dbg !70
  br label %11, !dbg !68

8:                                                ; preds = %1
  %9 = load i32, ptr %2, align 4, !dbg !71
  %10 = sub nsw i32 0, %9, !dbg !72
  br label %11, !dbg !68

11:                                               ; preds = %8, %6
  %12 = phi i32 [ %7, %6 ], [ %10, %8 ], !dbg !68
  store i32 %12, ptr %3, align 4, !dbg !67
  %13 = load i32, ptr %3, align 4, !dbg !73
  ret i32 %13, !dbg !74
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !41 !pallas.exprWrapper !75 {
  %2 = alloca %struct.s, align 8
  call void @llvm.dbg.value(metadata i32 %0, metadata !40, metadata !DIExpression()), !dbg !76
  call void @"pallas.result sret(%struct.s) align 8 void"(ptr sret(%struct.s) align 8 %2), !dbg !77
  %3 = getelementptr inbounds %struct.s, ptr %2, i32 0, i32 0, !dbg !78
  %4 = load i64, ptr %3, align 8, !dbg !78
  %5 = icmp sge i64 %4, 0, !dbg !79
  ret i1 %5, !dbg !76
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !64 !pallas.exprWrapper !75 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !63, metadata !DIExpression()), !dbg !80
  %2 = call i32 @"pallas.result i32"(), !dbg !81
  %3 = icmp sge i32 %2, 0, !dbg !82
  ret i1 %3, !dbg !80
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !83 void @"pallas.result sret(%struct.s) align 8 void"(ptr sret(%struct.s) align 8)

declare !pallas.specLib !83 i32 @"pallas.result i32"()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_result.c", directory: ".", checksumkind: CSK_MD5, checksum: "2f22e29e51ce30dd065cc36f09ad6eb8")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "c8bb2d17ff6ffc5b8e7b0eca1949de12")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "fun", scope: !1, file: !1, line: 29, type: !13, scopeLine: 29, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!13 = !DISubroutineType(types: !14)
!14 = !{!15, !30}
!15 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !1, line: 17, baseType: !16)
!16 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "s", file: !1, line: 9, size: 448, elements: !17)
!17 = !{!18, !24, !25, !26, !27, !28, !29}
!18 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !16, file: !1, line: 10, baseType: !19, size: 64)
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !20, line: 27, baseType: !21)
!20 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!21 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !22, line: 44, baseType: !23)
!22 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!23 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!24 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !16, file: !1, line: 11, baseType: !19, size: 64, offset: 64)
!25 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !16, file: !1, line: 12, baseType: !19, size: 64, offset: 128)
!26 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !16, file: !1, line: 13, baseType: !19, size: 64, offset: 192)
!27 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !16, file: !1, line: 14, baseType: !19, size: 64, offset: 256)
!28 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !16, file: !1, line: 15, baseType: !19, size: 64, offset: 320)
!29 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !16, file: !1, line: 16, baseType: !19, size: 64, offset: 384)
!30 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!31 = !{}
!32 = !{!33, i1 false, i1 false, !31, !31, !35}
!33 = !{!"pallas.srcLoc", i64 26, i64 1, i64 28, i64 1, !34}
!34 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_result.c", directory: "", checksumkind: CSK_MD5, checksum: "2f22e29e51ce30dd065cc36f09ad6eb8")
!35 = !{!"pallas.ensures", !36, ptr @PALLAS_SPEC_0, !31, !31, !37}
!36 = !{!"pallas.srcLoc", i64 27, i64 1, i64 27, i64 34, !34}
!37 = !{!38}
!38 = !{!39, !40}
!39 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 29, type: !30)
!40 = !DILocalVariable(name: "a", arg: 1, scope: !41, file: !1, line: 27, type: !30)
!41 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 27, type: !42, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!42 = !DISubroutineType(types: !43)
!43 = !{!44, !30}
!44 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!45 = !DILocation(line: 29, column: 20, scope: !12)
!46 = !DILocalVariable(name: "s", scope: !12, file: !1, line: 30, type: !15)
!47 = !DILocation(line: 30, column: 15, scope: !12)
!48 = !DILocation(line: 31, column: 7, scope: !12)
!49 = !DILocation(line: 31, column: 9, scope: !12)
!50 = !DILocation(line: 32, column: 7, scope: !12)
!51 = !DILocation(line: 32, column: 9, scope: !12)
!52 = !DILocation(line: 33, column: 5, scope: !12)
!53 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 40, type: !54, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!54 = !DISubroutineType(types: !55)
!55 = !{!30, !30}
!56 = !{!57, i1 false, i1 false, !31, !31, !58}
!57 = !{!"pallas.srcLoc", i64 37, i64 1, i64 39, i64 1, !34}
!58 = !{!"pallas.ensures", !59, ptr @PALLAS_SPEC_1, !31, !31, !60}
!59 = !{!"pallas.srcLoc", i64 38, i64 1, i64 38, i64 26, !34}
!60 = !{!61}
!61 = !{!62, !63}
!62 = !DILocalVariable(name: "x", arg: 1, scope: !53, file: !1, line: 40, type: !30)
!63 = !DILocalVariable(name: "x", arg: 1, scope: !64, file: !1, line: 38, type: !30)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 38, type: !42, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!65 = !DILocation(line: 40, column: 14, scope: !53)
!66 = !DILocalVariable(name: "y", scope: !53, file: !1, line: 41, type: !30)
!67 = !DILocation(line: 41, column: 9, scope: !53)
!68 = !DILocation(line: 41, column: 13, scope: !53)
!69 = !DILocation(line: 41, column: 15, scope: !53)
!70 = !DILocation(line: 41, column: 21, scope: !53)
!71 = !DILocation(line: 41, column: 26, scope: !53)
!72 = !DILocation(line: 41, column: 25, scope: !53)
!73 = !DILocation(line: 42, column: 12, scope: !53)
!74 = !DILocation(line: 42, column: 5, scope: !53)
!75 = !{!""}
!76 = !DILocation(line: 0, scope: !41)
!77 = !DILocation(line: 27, column: 9, scope: !41)
!78 = !DILocation(line: 27, column: 28, scope: !41)
!79 = !DILocation(line: 27, column: 30, scope: !41)
!80 = !DILocation(line: 0, scope: !64)
!81 = !DILocation(line: 38, column: 9, scope: !64)
!82 = !DILocation(line: 38, column: 22, scope: !64)
!83 = !{!"pallas.result"}
