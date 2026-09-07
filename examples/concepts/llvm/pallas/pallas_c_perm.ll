; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_c_perm.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.S = type { i64, i64, i64, i64, i64, i64, i64 }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [6 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5], section "llvm.metadata"

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
define dso_local void @bar(ptr noundef %0) #0 !dbg !44 !pallas.fcontract !63 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !69, metadata !DIExpression()), !dbg !103
  %3 = load ptr, ptr %2, align 8, !dbg !104
  %4 = getelementptr inbounds %struct.S, ptr %3, i32 0, i32 0, !dbg !105
  store i64 0, ptr %4, align 8, !dbg !106
  %5 = load ptr, ptr %2, align 8, !dbg !107
  %6 = getelementptr inbounds %struct.S, ptr %5, i32 0, i32 1, !dbg !108
  store i64 0, ptr %6, align 8, !dbg !109
  ret void, !dbg !110
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !29 !pallas.exprWrapper !111 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !28, metadata !DIExpression()), !dbg !112
  %3 = icmp ne ptr %0, null, !dbg !113
  br i1 %3, label %4, label %6, !dbg !114

4:                                                ; preds = %1
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !115
  %5 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !116
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i1 [ false, %1 ], [ %5, %4 ], !dbg !112
  ret i1 %7, !dbg !112
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !38 !pallas.exprWrapper !111 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !37, metadata !DIExpression()), !dbg !117
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 2), !dbg !118
  %3 = call i1 @pallas.perm(ptr noundef %0, ptr noundef byval(%pallas.fracT) %2), !dbg !119
  ret i1 %3, !dbg !117
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !71 !pallas.exprWrapper !111 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !70, metadata !DIExpression()), !dbg !120
  %2 = icmp ne ptr %0, null, !dbg !121
  ret i1 %2, !dbg !120
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !90 !pallas.exprWrapper !111 {
  %2 = alloca %pallas.fracT, align 8
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !89, metadata !DIExpression()), !dbg !122
  %4 = getelementptr inbounds %struct.S, ptr %0, i32 0, i32 0, !dbg !123
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !124
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %2), !dbg !125
  %6 = getelementptr inbounds %struct.S, ptr %0, i32 0, i32 1, !dbg !126
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !127
  %7 = call i1 @pallas.perm(ptr noundef %6, ptr noundef byval(%pallas.fracT) %3), !dbg !128
  %8 = call i1 @pallas.sepConj(i1 %5, i1 %7), !dbg !129
  ret i1 %8, !dbg !122
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0) #0 !dbg !96 !pallas.exprWrapper !111 {
  %2 = alloca %pallas.fracT, align 8
  %3 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.value(metadata ptr %0, metadata !95, metadata !DIExpression()), !dbg !130
  %4 = getelementptr inbounds %struct.S, ptr %0, i32 0, i32 0, !dbg !131
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !132
  %5 = call i1 @pallas.perm(ptr noundef %4, ptr noundef byval(%pallas.fracT) %2), !dbg !133
  %6 = getelementptr inbounds %struct.S, ptr %0, i32 0, i32 1, !dbg !134
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %3, i32 noundef 1, i32 noundef 1), !dbg !135
  %7 = call i1 @pallas.perm(ptr noundef %6, ptr noundef byval(%pallas.fracT) %3), !dbg !136
  %8 = call i1 @pallas.sepConj(i1 %5, i1 %7), !dbg !137
  ret i1 %8, !dbg !130
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0) #0 !dbg !102 !pallas.exprWrapper !111 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !101, metadata !DIExpression()), !dbg !138
  %2 = getelementptr inbounds %struct.S, ptr %0, i32 0, i32 0, !dbg !139
  %3 = load i64, ptr %2, align 8, !dbg !139
  %4 = icmp eq i64 %3, 0, !dbg !140
  br i1 %4, label %5, label %9, !dbg !141

5:                                                ; preds = %1
  %6 = getelementptr inbounds %struct.S, ptr %0, i32 0, i32 1, !dbg !142
  %7 = load i64, ptr %6, align 8, !dbg !142
  %8 = icmp eq i64 %7, 0, !dbg !143
  br label %9

9:                                                ; preds = %5, %1
  %10 = phi i1 [ false, %1 ], [ %8, %5 ], !dbg !138
  ret i1 %10, !dbg !138
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !144 i1 @pallas.sepConj(i1, i1)

declare !pallas.specLib !145 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !146 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!6, !7, !8, !9, !10, !11, !12}
!llvm.ident = !{!13, !13}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_c_perm.c", directory: ".", checksumkind: CSK_MD5, checksum: "06af036f7ddb85158b950fc5ea26d4e8")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "5f6e565f4d11d3131fb076d28e6cc0b5")
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
!22 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_c_perm.c", directory: "", checksumkind: CSK_MD5, checksum: "06af036f7ddb85158b950fc5ea26d4e8")
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
!44 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 22, type: !45, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!45 = !DISubroutineType(types: !46)
!46 = !{null, !47}
!47 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !48, size: 64)
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !1, line: 14, baseType: !49)
!49 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "S", file: !1, line: 12, size: 448, elements: !50)
!50 = !{!51, !57, !58, !59, !60, !61, !62}
!51 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !49, file: !1, line: 13, baseType: !52, size: 64)
!52 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !53, line: 27, baseType: !54)
!53 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!54 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !55, line: 44, baseType: !56)
!55 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!56 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!57 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !49, file: !1, line: 13, baseType: !52, size: 64, offset: 64)
!58 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !49, file: !1, line: 13, baseType: !52, size: 64, offset: 128)
!59 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !49, file: !1, line: 13, baseType: !52, size: 64, offset: 192)
!60 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !49, file: !1, line: 13, baseType: !52, size: 64, offset: 256)
!61 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !49, file: !1, line: 13, baseType: !52, size: 64, offset: 320)
!62 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !49, file: !1, line: 13, baseType: !52, size: 64, offset: 384)
!63 = !{!64, i1 false, i1 false, !19, !19, !65, !85, !91, !97}
!64 = !{!"pallas.srcLoc", i64 16, i64 1, i64 21, i64 1, !22}
!65 = !{!"pallas.requires", !66, ptr @PALLAS_SPEC_2, !19, !19, !67}
!66 = !{!"pallas.srcLoc", i64 17, i64 1, i64 17, i64 19, !22}
!67 = !{!68}
!68 = !{!69, !70}
!69 = !DILocalVariable(name: "s", arg: 1, scope: !44, file: !1, line: 22, type: !47)
!70 = !DILocalVariable(name: "s", arg: 1, scope: !71, file: !1, line: 17, type: !74)
!71 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 17, type: !72, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!72 = !DISubroutineType(types: !73)
!73 = !{!32, !74}
!74 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !75, size: 64)
!75 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !3, line: 33, baseType: !76)
!76 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "S", file: !3, line: 31, size: 448, elements: !77)
!77 = !{!78, !79, !80, !81, !82, !83, !84}
!78 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !76, file: !3, line: 32, baseType: !52, size: 64)
!79 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !76, file: !3, line: 32, baseType: !52, size: 64, offset: 64)
!80 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !76, file: !3, line: 32, baseType: !52, size: 64, offset: 128)
!81 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !76, file: !3, line: 32, baseType: !52, size: 64, offset: 192)
!82 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !76, file: !3, line: 32, baseType: !52, size: 64, offset: 256)
!83 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !76, file: !3, line: 32, baseType: !52, size: 64, offset: 320)
!84 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !76, file: !3, line: 32, baseType: !52, size: 64, offset: 384)
!85 = !{!"pallas.requires", !86, ptr @PALLAS_SPEC_3, !19, !19, !87}
!86 = !{!"pallas.srcLoc", i64 18, i64 1, i64 18, i64 58, !22}
!87 = !{!88}
!88 = !{!69, !89}
!89 = !DILocalVariable(name: "s", arg: 1, scope: !90, file: !1, line: 18, type: !74)
!90 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 18, type: !72, scopeLine: 18, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!91 = !{!"pallas.ensures", !92, ptr @PALLAS_SPEC_4, !19, !19, !93}
!92 = !{!"pallas.srcLoc", i64 19, i64 1, i64 19, i64 57, !22}
!93 = !{!94}
!94 = !{!69, !95}
!95 = !DILocalVariable(name: "s", arg: 1, scope: !96, file: !1, line: 19, type: !74)
!96 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 19, type: !72, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!97 = !{!"pallas.ensures", !98, ptr @PALLAS_SPEC_5, !19, !19, !99}
!98 = !{!"pallas.srcLoc", i64 20, i64 1, i64 20, i64 31, !22}
!99 = !{!100}
!100 = !{!69, !101}
!101 = !DILocalVariable(name: "s", arg: 1, scope: !102, file: !1, line: 20, type: !74)
!102 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 20, type: !72, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !19)
!103 = !DILocation(line: 22, column: 21, scope: !44)
!104 = !DILocation(line: 23, column: 5, scope: !44)
!105 = !DILocation(line: 23, column: 8, scope: !44)
!106 = !DILocation(line: 23, column: 10, scope: !44)
!107 = !DILocation(line: 24, column: 5, scope: !44)
!108 = !DILocation(line: 24, column: 8, scope: !44)
!109 = !DILocation(line: 24, column: 10, scope: !44)
!110 = !DILocation(line: 25, column: 1, scope: !44)
!111 = !{!""}
!112 = !DILocation(line: 0, scope: !29)
!113 = !DILocation(line: 5, column: 14, scope: !29)
!114 = !DILocation(line: 5, column: 22, scope: !29)
!115 = !DILocation(line: 5, column: 36, scope: !29)
!116 = !DILocation(line: 5, column: 25, scope: !29)
!117 = !DILocation(line: 0, scope: !38)
!118 = !DILocation(line: 6, column: 20, scope: !38)
!119 = !DILocation(line: 6, column: 9, scope: !38)
!120 = !DILocation(line: 0, scope: !71)
!121 = !DILocation(line: 17, column: 12, scope: !71)
!122 = !DILocation(line: 0, scope: !90)
!123 = !DILocation(line: 18, column: 25, scope: !90)
!124 = !DILocation(line: 18, column: 28, scope: !90)
!125 = !DILocation(line: 18, column: 15, scope: !90)
!126 = !DILocation(line: 18, column: 47, scope: !90)
!127 = !DILocation(line: 18, column: 50, scope: !90)
!128 = !DILocation(line: 18, column: 37, scope: !90)
!129 = !DILocation(line: 18, column: 10, scope: !90)
!130 = !DILocation(line: 0, scope: !96)
!131 = !DILocation(line: 19, column: 24, scope: !96)
!132 = !DILocation(line: 19, column: 27, scope: !96)
!133 = !DILocation(line: 19, column: 14, scope: !96)
!134 = !DILocation(line: 19, column: 46, scope: !96)
!135 = !DILocation(line: 19, column: 49, scope: !96)
!136 = !DILocation(line: 19, column: 36, scope: !96)
!137 = !DILocation(line: 19, column: 9, scope: !96)
!138 = !DILocation(line: 0, scope: !102)
!139 = !DILocation(line: 20, column: 12, scope: !102)
!140 = !DILocation(line: 20, column: 14, scope: !102)
!141 = !DILocation(line: 20, column: 19, scope: !102)
!142 = !DILocation(line: 20, column: 25, scope: !102)
!143 = !DILocation(line: 20, column: 27, scope: !102)
!144 = !{!"pallas.sepConj"}
!145 = !{!"pallas.perm"}
!146 = !{!"pallas.fracOf"}
