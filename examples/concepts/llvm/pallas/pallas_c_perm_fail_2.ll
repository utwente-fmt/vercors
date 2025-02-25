; ModuleID = './tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_perm_fail_2.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.S = type { i64, i64, i64, i64, i64, i64, i64 }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [3 x ptr] [ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @bar(ptr noundef %0) #0 !dbg !14 !pallas.fcontract !34 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !38, metadata !DIExpression()), !dbg !43
  %3 = load ptr, ptr %2, align 8, !dbg !44
  %4 = getelementptr inbounds %struct.S, ptr %3, i32 0, i32 0, !dbg !45
  store i64 0, ptr %4, align 8, !dbg !46
  ret void, !dbg !47
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !48 !pallas.exprWrapper !64 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !65, metadata !DIExpression()), !dbg !66
  %2 = getelementptr inbounds %struct.S, ptr %0, i32 0, i32 0, !dbg !67
  %3 = load i64, ptr %2, align 8, !dbg !67
  %4 = icmp eq i64 %3, 0, !dbg !68
  ret i1 %4, !dbg !66
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !69 !pallas.exprWrapper !64 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !70, metadata !DIExpression()), !dbg !71
  %3 = icmp ne ptr %0, null, !dbg !72
  br i1 %3, label %4, label %6, !dbg !73

4:                                                ; preds = %1
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !74
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !75
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i1 [ false, %1 ], [ %5, %4 ], !dbg !71
  ret i1 %7, !dbg !71
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !76 !pallas.exprWrapper !64 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !77, metadata !DIExpression()), !dbg !78
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !79
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !80
  ret i1 %3, !dbg !78
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !81 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !82 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}
!llvm.ident = !{!13, !13}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_perm_fail_2.c", directory: ".", checksumkind: CSK_MD5, checksum: "a1fc853c50100ff6c9de9d333db797b7")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "3ecacb47795eb31d75b75313e691d298")
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
!14 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 14, type: !15, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !33)
!15 = !DISubroutineType(types: !16)
!16 = !{null, !17}
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64)
!18 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !1, line: 7, baseType: !19)
!19 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "S", file: !1, line: 5, size: 448, elements: !20)
!20 = !{!21, !27, !28, !29, !30, !31, !32}
!21 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !19, file: !1, line: 6, baseType: !22, size: 64)
!22 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !23, line: 27, baseType: !24)
!23 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!24 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !25, line: 44, baseType: !26)
!25 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!26 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!27 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 64)
!28 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 128)
!29 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 192)
!30 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 256)
!31 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 320)
!32 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !19, file: !1, line: 6, baseType: !22, size: 64, offset: 384)
!33 = !{}
!34 = !{!35, i1 false, !36, !39, !41}
!35 = !{!"pallas.srcLoc", i64 9, i64 1, i64 13, i64 1}
!36 = !{!"pallas.requires", !37, ptr @PALLAS_SPEC_0, !38}
!37 = !{!"pallas.srcLoc", i64 10, i64 1, i64 10, i64 44}
!38 = !DILocalVariable(name: "s", arg: 1, scope: !14, file: !1, line: 14, type: !17)
!39 = !{!"pallas.ensures", !40, ptr @PALLAS_SPEC_1, !38}
!40 = !{!"pallas.srcLoc", i64 11, i64 1, i64 11, i64 30}
!41 = !{!"pallas.ensures", !42, ptr @PALLAS_SPEC_2, !38}
!42 = !{!"pallas.srcLoc", i64 12, i64 1, i64 12, i64 18}
!43 = !DILocation(line: 14, column: 21, scope: !14)
!44 = !DILocation(line: 15, column: 5, scope: !14)
!45 = !DILocation(line: 15, column: 8, scope: !14)
!46 = !DILocation(line: 15, column: 10, scope: !14)
!47 = !DILocation(line: 16, column: 1, scope: !14)
!48 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 12, type: !49, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !33)
!49 = !DISubroutineType(types: !50)
!50 = !{!51, !52}
!51 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!52 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !53, size: 64)
!53 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !54, line: 8, baseType: !55)
!54 = !DIFile(filename: "./tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "3ecacb47795eb31d75b75313e691d298")
!55 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "S", file: !54, line: 6, size: 448, elements: !56)
!56 = !{!57, !58, !59, !60, !61, !62, !63}
!57 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !55, file: !54, line: 7, baseType: !22, size: 64)
!58 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !55, file: !54, line: 7, baseType: !22, size: 64, offset: 64)
!59 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !55, file: !54, line: 7, baseType: !22, size: 64, offset: 128)
!60 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !55, file: !54, line: 7, baseType: !22, size: 64, offset: 192)
!61 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !55, file: !54, line: 7, baseType: !22, size: 64, offset: 256)
!62 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !55, file: !54, line: 7, baseType: !22, size: 64, offset: 320)
!63 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !55, file: !54, line: 7, baseType: !22, size: 64, offset: 384)
!64 = !{!""}
!65 = !DILocalVariable(name: "s", arg: 1, scope: !48, file: !1, line: 12, type: !52)
!66 = !DILocation(line: 0, scope: !48)
!67 = !DILocation(line: 12, column: 12, scope: !48)
!68 = !DILocation(line: 12, column: 14, scope: !48)
!69 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 10, type: !49, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !33)
!70 = !DILocalVariable(name: "s", arg: 1, scope: !69, file: !1, line: 10, type: !52)
!71 = !DILocation(line: 0, scope: !69)
!72 = !DILocation(line: 10, column: 12, scope: !69)
!73 = !DILocation(line: 10, column: 20, scope: !69)
!74 = !DILocation(line: 10, column: 31, scope: !69)
!75 = !DILocation(line: 10, column: 23, scope: !69)
!76 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 11, type: !49, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !33)
!77 = !DILocalVariable(name: "s", arg: 1, scope: !76, file: !1, line: 11, type: !52)
!78 = !DILocation(line: 0, scope: !76)
!79 = !DILocation(line: 11, column: 17, scope: !76)
!80 = !DILocation(line: 11, column: 9, scope: !76)
!81 = !{!"pallas.perm"}
!82 = !{!"pallas.fracOf"}
