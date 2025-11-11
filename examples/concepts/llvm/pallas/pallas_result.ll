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
  call void @llvm.dbg.declare(metadata ptr %3, metadata !37, metadata !DIExpression()), !dbg !38
  call void @llvm.dbg.declare(metadata ptr %0, metadata !39, metadata !DIExpression()), !dbg !40
  %4 = getelementptr inbounds %struct.s, ptr %0, i32 0, i32 0, !dbg !41
  store i64 0, ptr %4, align 8, !dbg !42
  %5 = getelementptr inbounds %struct.s, ptr %0, i32 0, i32 1, !dbg !43
  store i64 1, ptr %5, align 8, !dbg !44
  ret void, !dbg !45
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @bar(i32 noundef %0) #0 !dbg !46 !pallas.fcontract !49 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !53, metadata !DIExpression()), !dbg !54
  call void @llvm.dbg.declare(metadata ptr %3, metadata !55, metadata !DIExpression()), !dbg !56
  %4 = load i32, ptr %2, align 4, !dbg !57
  %5 = icmp sgt i32 %4, 0, !dbg !58
  br i1 %5, label %6, label %8, !dbg !57

6:                                                ; preds = %1
  %7 = load i32, ptr %2, align 4, !dbg !59
  br label %11, !dbg !57

8:                                                ; preds = %1
  %9 = load i32, ptr %2, align 4, !dbg !60
  %10 = sub nsw i32 0, %9, !dbg !61
  br label %11, !dbg !57

11:                                               ; preds = %8, %6
  %12 = phi i32 [ %7, %6 ], [ %10, %8 ], !dbg !57
  store i32 %12, ptr %3, align 4, !dbg !56
  %13 = load i32, ptr %3, align 4, !dbg !62
  ret i32 %13, !dbg !63
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(i32 noundef %0) #0 !dbg !64 !pallas.exprWrapper !68 {
  %2 = alloca %struct.s, align 8
  call void @llvm.dbg.value(metadata i32 %0, metadata !69, metadata !DIExpression()), !dbg !70
  call void @pallas.result.0(ptr sret(%struct.s) align 8 %2), !dbg !71
  %3 = getelementptr inbounds %struct.s, ptr %2, i32 0, i32 0, !dbg !72
  %4 = load i64, ptr %3, align 8, !dbg !72
  %5 = icmp sge i64 %4, 0, !dbg !73
  ret i1 %5, !dbg !70
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(i32 noundef %0) #0 !dbg !74 !pallas.exprWrapper !68 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !75, metadata !DIExpression()), !dbg !76
  %2 = call i32 @pallas.result.1(), !dbg !77
  %3 = icmp sge i32 %2, 0, !dbg !78
  ret i1 %3, !dbg !76
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !79 void @pallas.result.0(ptr sret(%struct.s) align 8)

declare !pallas.specLib !79 i32 @pallas.result.1()

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
!32 = !{!33, i1 false, i1 false, !35}
!33 = !{!"pallas.srcLoc", i64 26, i64 1, i64 28, i64 1, !34}
!34 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_result.c", directory: "", checksumkind: CSK_MD5, checksum: "2f22e29e51ce30dd065cc36f09ad6eb8")
!35 = !{!"pallas.ensures", !36, ptr @PALLAS_SPEC_0, !37}
!36 = !{!"pallas.srcLoc", i64 27, i64 1, i64 27, i64 34, !34}
!37 = !DILocalVariable(name: "a", arg: 1, scope: !12, file: !1, line: 29, type: !30)
!38 = !DILocation(line: 29, column: 20, scope: !12)
!39 = !DILocalVariable(name: "s", scope: !12, file: !1, line: 30, type: !15)
!40 = !DILocation(line: 30, column: 15, scope: !12)
!41 = !DILocation(line: 31, column: 7, scope: !12)
!42 = !DILocation(line: 31, column: 9, scope: !12)
!43 = !DILocation(line: 32, column: 7, scope: !12)
!44 = !DILocation(line: 32, column: 9, scope: !12)
!45 = !DILocation(line: 33, column: 5, scope: !12)
!46 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 40, type: !47, scopeLine: 40, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!47 = !DISubroutineType(types: !48)
!48 = !{!30, !30}
!49 = !{!50, i1 false, i1 false, !51}
!50 = !{!"pallas.srcLoc", i64 37, i64 1, i64 39, i64 1, !34}
!51 = !{!"pallas.ensures", !52, ptr @PALLAS_SPEC_1, !53}
!52 = !{!"pallas.srcLoc", i64 38, i64 1, i64 38, i64 26, !34}
!53 = !DILocalVariable(name: "x", arg: 1, scope: !46, file: !1, line: 40, type: !30)
!54 = !DILocation(line: 40, column: 14, scope: !46)
!55 = !DILocalVariable(name: "y", scope: !46, file: !1, line: 41, type: !30)
!56 = !DILocation(line: 41, column: 9, scope: !46)
!57 = !DILocation(line: 41, column: 13, scope: !46)
!58 = !DILocation(line: 41, column: 15, scope: !46)
!59 = !DILocation(line: 41, column: 21, scope: !46)
!60 = !DILocation(line: 41, column: 26, scope: !46)
!61 = !DILocation(line: 41, column: 25, scope: !46)
!62 = !DILocation(line: 42, column: 12, scope: !46)
!63 = !DILocation(line: 42, column: 5, scope: !46)
!64 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 27, type: !65, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!65 = !DISubroutineType(types: !66)
!66 = !{!67, !30}
!67 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!68 = !{!""}
!69 = !DILocalVariable(name: "a", arg: 1, scope: !64, file: !1, line: 27, type: !30)
!70 = !DILocation(line: 0, scope: !64)
!71 = !DILocation(line: 27, column: 9, scope: !64)
!72 = !DILocation(line: 27, column: 28, scope: !64)
!73 = !DILocation(line: 27, column: 30, scope: !64)
!74 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 38, type: !65, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !31)
!75 = !DILocalVariable(name: "x", arg: 1, scope: !74, file: !1, line: 38, type: !30)
!76 = !DILocation(line: 0, scope: !74)
!77 = !DILocation(line: 38, column: 9, scope: !74)
!78 = !DILocation(line: 38, column: 22, scope: !74)
!79 = !{!"pallas.result"}
