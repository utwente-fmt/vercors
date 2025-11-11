; ModuleID = 'tmp_ir_source0.ll'
source_filename = "examples/concepts/c/pointer_relations.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.A = type { %struct.B, i8, i32, float }
%struct.B = type { i32, float, i8 }

@llvm.used = appending global [16 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @test1(ptr noundef %0) #0 !dbg !17 !pallas.fcontract !36 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !40, metadata !DIExpression()), !dbg !41
  ret void, !dbg !42, !pallas.stmntBlock !43
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @test2(ptr noundef %0) #0 !dbg !47 !pallas.fcontract !48 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !52, metadata !DIExpression()), !dbg !53
  ret void, !dbg !54, !pallas.stmntBlock !55
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test3(ptr noundef %0) #0 !dbg !59 !pallas.fcontract !60 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !64, metadata !DIExpression()), !dbg !65
  ret void, !dbg !66, !pallas.stmntBlock !67
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test4(ptr noundef %0) #0 !dbg !71 !pallas.fcontract !72 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !76, metadata !DIExpression()), !dbg !77
  ret void, !dbg !78, !pallas.stmntBlock !79
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test5(ptr noundef %0) #0 !dbg !83 !pallas.fcontract !84 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !88, metadata !DIExpression()), !dbg !89
  ret void, !dbg !90, !pallas.stmntBlock !91
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test6(ptr noundef %0) #0 !dbg !95 !pallas.fcontract !96 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !100, metadata !DIExpression()), !dbg !101
  ret void, !dbg !102, !pallas.stmntBlock !103
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test7(ptr noundef %0) #0 !dbg !107 !pallas.fcontract !108 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !112, metadata !DIExpression()), !dbg !113
  ret void, !dbg !114, !pallas.stmntBlock !115
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test8(ptr noundef %0) #0 !dbg !119 !pallas.fcontract !120 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !124, metadata !DIExpression()), !dbg !125
  ret void, !dbg !126, !pallas.stmntBlock !127
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !131 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !149, metadata !DIExpression()), !dbg !150
  %2 = icmp ne ptr %0, null, !dbg !151
  ret i1 %2, !dbg !150
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !152 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !153, metadata !DIExpression()), !dbg !154
  %2 = icmp ne ptr %0, null, !dbg !155
  ret i1 %2, !dbg !154
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !156 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !157, metadata !DIExpression()), !dbg !158
  %2 = icmp ne ptr %0, null, !dbg !159
  ret i1 %2, !dbg !158
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !160 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !161, metadata !DIExpression()), !dbg !162
  %2 = icmp ne ptr %0, null, !dbg !163
  ret i1 %2, !dbg !162
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0) #0 !dbg !164 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !165, metadata !DIExpression()), !dbg !166
  %2 = icmp ne ptr %0, null, !dbg !167
  ret i1 %2, !dbg !166
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0) #0 !dbg !168 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !169, metadata !DIExpression()), !dbg !170
  %2 = icmp ne ptr %0, null, !dbg !171
  ret i1 %2, !dbg !170
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0) #0 !dbg !172 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !173, metadata !DIExpression()), !dbg !174
  %2 = icmp ne ptr %0, null, !dbg !175
  ret i1 %2, !dbg !174
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0) #0 !dbg !176 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !177, metadata !DIExpression()), !dbg !178
  %2 = icmp ne ptr %0, null, !dbg !179
  ret i1 %2, !dbg !178
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0) #0 !dbg !180 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !181, metadata !DIExpression()), !dbg !182
  %2 = ptrtoint ptr %0 to i64, !dbg !183
  %3 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !184
  %4 = ptrtoint ptr %3 to i64, !dbg !185
  %5 = icmp eq i64 %2, %4, !dbg !186
  ret i1 %5, !dbg !182
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0) #0 !dbg !187 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !188, metadata !DIExpression()), !dbg !189
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !190
  %3 = ptrtoint ptr %2 to i64, !dbg !191
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 1, !dbg !192
  %5 = ptrtoint ptr %4 to i64, !dbg !193
  %6 = icmp ult i64 %3, %5, !dbg !194
  ret i1 %6, !dbg !189
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0) #0 !dbg !195 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !196, metadata !DIExpression()), !dbg !197
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 1, !dbg !198
  %3 = ptrtoint ptr %2 to i64, !dbg !199
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 2, !dbg !200
  %5 = ptrtoint ptr %4 to i64, !dbg !201
  %6 = icmp ult i64 %3, %5, !dbg !202
  ret i1 %6, !dbg !197
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0) #0 !dbg !203 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !204, metadata !DIExpression()), !dbg !205
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 2, !dbg !206
  %3 = ptrtoint ptr %2 to i64, !dbg !207
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 3, !dbg !208
  %5 = ptrtoint ptr %4 to i64, !dbg !209
  %6 = icmp ult i64 %3, %5, !dbg !210
  ret i1 %6, !dbg !205
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0) #0 !dbg !211 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !212, metadata !DIExpression()), !dbg !213
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !214
  %3 = ptrtoint ptr %2 to i64, !dbg !215
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !216
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0, !dbg !217
  %6 = ptrtoint ptr %5 to i64, !dbg !218
  %7 = icmp eq i64 %3, %6, !dbg !219
  ret i1 %7, !dbg !213
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0) #0 !dbg !220 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !221, metadata !DIExpression()), !dbg !222
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !223
  %3 = getelementptr inbounds %struct.B, ptr %2, i32 0, i32 0, !dbg !224
  %4 = ptrtoint ptr %3 to i64, !dbg !225
  %5 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 1, !dbg !226
  %6 = ptrtoint ptr %5 to i64, !dbg !227
  %7 = icmp ult i64 %4, %6, !dbg !228
  ret i1 %7, !dbg !222
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0) #0 !dbg !229 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !230, metadata !DIExpression()), !dbg !231
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !232
  %3 = getelementptr inbounds %struct.B, ptr %2, i32 0, i32 1, !dbg !233
  %4 = ptrtoint ptr %3 to i64, !dbg !234
  %5 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 2, !dbg !235
  %6 = ptrtoint ptr %5 to i64, !dbg !236
  %7 = icmp ult i64 %4, %6, !dbg !237
  ret i1 %7, !dbg !231
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0) #0 !dbg !238 !pallas.exprWrapper !148 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !239, metadata !DIExpression()), !dbg !240
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !241
  %3 = getelementptr inbounds %struct.B, ptr %2, i32 0, i32 2, !dbg !242
  %4 = ptrtoint ptr %3 to i64, !dbg !243
  %5 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 3, !dbg !244
  %6 = ptrtoint ptr %5 to i64, !dbg !245
  %7 = icmp ult i64 %4, %6, !dbg !246
  ret i1 %7, !dbg !240
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!9, !10, !11, !12, !13, !14, !15}
!llvm.ident = !{!16, !16}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "pointer_relations.c", directory: ".", checksumkind: CSK_MD5, checksum: "6d634623b3efb2fa906e17a0980f974d")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "5967dc9ce63807dabdd0aa72ebf4cd4d")
!4 = !{!5, !6}
!5 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!6 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !7, line: 79, baseType: !8)
!7 = !DIFile(filename: "/usr/include/stdint.h", directory: "", checksumkind: CSK_MD5, checksum: "bfb03fa9c46a839e35c32b929fbdbb8e")
!8 = !DIBasicType(name: "unsigned long", size: 64, encoding: DW_ATE_unsigned)
!9 = !{i32 7, !"Dwarf Version", i32 5}
!10 = !{i32 2, !"Debug Info Version", i32 3}
!11 = !{i32 1, !"wchar_size", i32 4}
!12 = !{i32 8, !"PIC Level", i32 2}
!13 = !{i32 7, !"PIE Level", i32 2}
!14 = !{i32 7, !"uwtable", i32 2}
!15 = !{i32 7, !"frame-pointer", i32 2}
!16 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!17 = distinct !DISubprogram(name: "test1", scope: !1, file: !1, line: 22, type: !18, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!18 = !DISubroutineType(types: !19)
!19 = !{null, !20}
!20 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !21, size: 64)
!21 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !1, line: 11, size: 192, elements: !22)
!22 = !{!23, !32, !33, !34}
!23 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !21, file: !1, line: 12, baseType: !24, size: 96)
!24 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "B", file: !1, line: 3, size: 96, elements: !25)
!25 = !{!26, !28, !30}
!26 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !24, file: !1, line: 4, baseType: !27, size: 32)
!27 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!28 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !24, file: !1, line: 5, baseType: !29, size: 32, offset: 32)
!29 = !DIBasicType(name: "float", size: 32, encoding: DW_ATE_float)
!30 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !24, file: !1, line: 6, baseType: !31, size: 8, offset: 64)
!31 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!32 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !21, file: !1, line: 13, baseType: !31, size: 8, offset: 96)
!33 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !21, file: !1, line: 14, baseType: !27, size: 32, offset: 128)
!34 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !21, file: !1, line: 15, baseType: !29, size: 32, offset: 160)
!35 = !{}
!36 = !{!37, i1 false, !38}
!37 = !{!"pallas.srcLoc", i64 21, i64 1, i64 21, i64 24}
!38 = !{!"pallas.requires", !39, ptr @PALLAS_SPEC_0, !40}
!39 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 23}
!40 = !DILocalVariable(name: "s", arg: 1, scope: !17, file: !1, line: 22, type: !20)
!41 = !DILocation(line: 22, column: 22, scope: !17)
!42 = !DILocation(line: 24, column: 1, scope: !17)
!43 = !{!44, !45}
!44 = !{!"pallas.srcLoc", i64 23, i64 5, i64 23, i64 49}
!45 = !{!"pallas.assert", !46, ptr @PALLAS_SPEC_8, !40}
!46 = !{!"pallas.srcLoc", i64 23, i64 9, i64 23, i64 48}
!47 = distinct !DISubprogram(name: "test2", scope: !1, file: !1, line: 27, type: !18, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!48 = !{!49, i1 false, !50}
!49 = !{!"pallas.srcLoc", i64 26, i64 1, i64 26, i64 24}
!50 = !{!"pallas.requires", !51, ptr @PALLAS_SPEC_1, !52}
!51 = !{!"pallas.srcLoc", i64 26, i64 5, i64 26, i64 23}
!52 = !DILocalVariable(name: "s", arg: 1, scope: !47, file: !1, line: 27, type: !20)
!53 = !DILocation(line: 27, column: 22, scope: !47)
!54 = !DILocation(line: 29, column: 1, scope: !47)
!55 = !{!56, !57}
!56 = !{!"pallas.srcLoc", i64 28, i64 5, i64 28, i64 53}
!57 = !{!"pallas.assert", !58, ptr @PALLAS_SPEC_9, !52}
!58 = !{!"pallas.srcLoc", i64 28, i64 9, i64 28, i64 51}
!59 = distinct !DISubprogram(name: "test3", scope: !1, file: !1, line: 32, type: !18, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!60 = !{!61, i1 false, !62}
!61 = !{!"pallas.srcLoc", i64 31, i64 1, i64 31, i64 24}
!62 = !{!"pallas.requires", !63, ptr @PALLAS_SPEC_2, !64}
!63 = !{!"pallas.srcLoc", i64 31, i64 5, i64 31, i64 23}
!64 = !DILocalVariable(name: "s", arg: 1, scope: !59, file: !1, line: 32, type: !20)
!65 = !DILocation(line: 32, column: 22, scope: !59)
!66 = !DILocation(line: 34, column: 1, scope: !59)
!67 = !{!68, !69}
!68 = !{!"pallas.srcLoc", i64 33, i64 5, i64 33, i64 53}
!69 = !{!"pallas.assert", !70, ptr @PALLAS_SPEC_10, !64}
!70 = !{!"pallas.srcLoc", i64 33, i64 9, i64 33, i64 51}
!71 = distinct !DISubprogram(name: "test4", scope: !1, file: !1, line: 37, type: !18, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!72 = !{!73, i1 false, !74}
!73 = !{!"pallas.srcLoc", i64 36, i64 1, i64 36, i64 24}
!74 = !{!"pallas.requires", !75, ptr @PALLAS_SPEC_3, !76}
!75 = !{!"pallas.srcLoc", i64 36, i64 5, i64 36, i64 23}
!76 = !DILocalVariable(name: "s", arg: 1, scope: !71, file: !1, line: 37, type: !20)
!77 = !DILocation(line: 37, column: 22, scope: !71)
!78 = !DILocation(line: 39, column: 1, scope: !71)
!79 = !{!80, !81}
!80 = !{!"pallas.srcLoc", i64 38, i64 5, i64 38, i64 53}
!81 = !{!"pallas.assert", !82, ptr @PALLAS_SPEC_11, !76}
!82 = !{!"pallas.srcLoc", i64 38, i64 9, i64 38, i64 51}
!83 = distinct !DISubprogram(name: "test5", scope: !1, file: !1, line: 42, type: !18, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!84 = !{!85, i1 false, !86}
!85 = !{!"pallas.srcLoc", i64 41, i64 1, i64 41, i64 24}
!86 = !{!"pallas.requires", !87, ptr @PALLAS_SPEC_4, !88}
!87 = !{!"pallas.srcLoc", i64 41, i64 5, i64 41, i64 23}
!88 = !DILocalVariable(name: "s", arg: 1, scope: !83, file: !1, line: 42, type: !20)
!89 = !DILocation(line: 42, column: 22, scope: !83)
!90 = !DILocation(line: 44, column: 1, scope: !83)
!91 = !{!92, !93}
!92 = !{!"pallas.srcLoc", i64 43, i64 5, i64 43, i64 56}
!93 = !{!"pallas.assert", !94, ptr @PALLAS_SPEC_12, !88}
!94 = !{!"pallas.srcLoc", i64 43, i64 9, i64 43, i64 54}
!95 = distinct !DISubprogram(name: "test6", scope: !1, file: !1, line: 47, type: !18, scopeLine: 47, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!96 = !{!97, i1 false, !98}
!97 = !{!"pallas.srcLoc", i64 46, i64 1, i64 46, i64 24}
!98 = !{!"pallas.requires", !99, ptr @PALLAS_SPEC_5, !100}
!99 = !{!"pallas.srcLoc", i64 46, i64 5, i64 46, i64 23}
!100 = !DILocalVariable(name: "s", arg: 1, scope: !95, file: !1, line: 47, type: !20)
!101 = !DILocation(line: 47, column: 22, scope: !95)
!102 = !DILocation(line: 49, column: 1, scope: !95)
!103 = !{!104, !105}
!104 = !{!"pallas.srcLoc", i64 48, i64 5, i64 48, i64 55}
!105 = !{!"pallas.assert", !106, ptr @PALLAS_SPEC_13, !100}
!106 = !{!"pallas.srcLoc", i64 48, i64 9, i64 48, i64 53}
!107 = distinct !DISubprogram(name: "test7", scope: !1, file: !1, line: 52, type: !18, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!108 = !{!109, i1 false, !110}
!109 = !{!"pallas.srcLoc", i64 51, i64 1, i64 51, i64 24}
!110 = !{!"pallas.requires", !111, ptr @PALLAS_SPEC_6, !112}
!111 = !{!"pallas.srcLoc", i64 51, i64 5, i64 51, i64 23}
!112 = !DILocalVariable(name: "s", arg: 1, scope: !107, file: !1, line: 52, type: !20)
!113 = !DILocation(line: 52, column: 22, scope: !107)
!114 = !DILocation(line: 54, column: 1, scope: !107)
!115 = !{!116, !117}
!116 = !{!"pallas.srcLoc", i64 53, i64 5, i64 53, i64 55}
!117 = !{!"pallas.assert", !118, ptr @PALLAS_SPEC_14, !112}
!118 = !{!"pallas.srcLoc", i64 53, i64 9, i64 53, i64 53}
!119 = distinct !DISubprogram(name: "test8", scope: !1, file: !1, line: 57, type: !18, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!120 = !{!121, i1 false, !122}
!121 = !{!"pallas.srcLoc", i64 56, i64 1, i64 56, i64 24}
!122 = !{!"pallas.requires", !123, ptr @PALLAS_SPEC_7, !124}
!123 = !{!"pallas.srcLoc", i64 56, i64 5, i64 56, i64 23}
!124 = !DILocalVariable(name: "s", arg: 1, scope: !119, file: !1, line: 57, type: !20)
!125 = !DILocation(line: 57, column: 22, scope: !119)
!126 = !DILocation(line: 59, column: 1, scope: !119)
!127 = !{!128, !129}
!128 = !{!"pallas.srcLoc", i64 58, i64 5, i64 58, i64 55}
!129 = !{!"pallas.assert", !130, ptr @PALLAS_SPEC_15, !124}
!130 = !{!"pallas.srcLoc", i64 58, i64 9, i64 58, i64 53}
!131 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 21, type: !132, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!132 = !DISubroutineType(types: !133)
!133 = !{!134, !135}
!134 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!135 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !136, size: 64)
!136 = !DIDerivedType(tag: DW_TAG_typedef, name: "A", file: !3, line: 19, baseType: !137)
!137 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !3, line: 12, size: 192, elements: !138)
!138 = !{!139, !145, !146, !147}
!139 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !137, file: !3, line: 13, baseType: !140, size: 96)
!140 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "B", file: !3, line: 4, size: 96, elements: !141)
!141 = !{!142, !143, !144}
!142 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !140, file: !3, line: 5, baseType: !27, size: 32)
!143 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !140, file: !3, line: 6, baseType: !29, size: 32, offset: 32)
!144 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !140, file: !3, line: 7, baseType: !31, size: 8, offset: 64)
!145 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !137, file: !3, line: 14, baseType: !31, size: 8, offset: 96)
!146 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !137, file: !3, line: 15, baseType: !27, size: 32, offset: 128)
!147 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !137, file: !3, line: 16, baseType: !29, size: 32, offset: 160)
!148 = !{!""}
!149 = !DILocalVariable(name: "s", arg: 1, scope: !131, file: !1, line: 21, type: !135)
!150 = !DILocation(line: 0, scope: !131)
!151 = !DILocation(line: 21, column: 16, scope: !131)
!152 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 26, type: !132, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!153 = !DILocalVariable(name: "s", arg: 1, scope: !152, file: !1, line: 26, type: !135)
!154 = !DILocation(line: 0, scope: !152)
!155 = !DILocation(line: 26, column: 16, scope: !152)
!156 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 31, type: !132, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!157 = !DILocalVariable(name: "s", arg: 1, scope: !156, file: !1, line: 31, type: !135)
!158 = !DILocation(line: 0, scope: !156)
!159 = !DILocation(line: 31, column: 16, scope: !156)
!160 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 36, type: !132, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!161 = !DILocalVariable(name: "s", arg: 1, scope: !160, file: !1, line: 36, type: !135)
!162 = !DILocation(line: 0, scope: !160)
!163 = !DILocation(line: 36, column: 16, scope: !160)
!164 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 41, type: !132, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!165 = !DILocalVariable(name: "s", arg: 1, scope: !164, file: !1, line: 41, type: !135)
!166 = !DILocation(line: 0, scope: !164)
!167 = !DILocation(line: 41, column: 16, scope: !164)
!168 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 46, type: !132, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!169 = !DILocalVariable(name: "s", arg: 1, scope: !168, file: !1, line: 46, type: !135)
!170 = !DILocation(line: 0, scope: !168)
!171 = !DILocation(line: 46, column: 16, scope: !168)
!172 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 51, type: !132, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!173 = !DILocalVariable(name: "s", arg: 1, scope: !172, file: !1, line: 51, type: !135)
!174 = !DILocation(line: 0, scope: !172)
!175 = !DILocation(line: 51, column: 16, scope: !172)
!176 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 56, type: !132, scopeLine: 56, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!177 = !DILocalVariable(name: "s", arg: 1, scope: !176, file: !1, line: 56, type: !135)
!178 = !DILocation(line: 0, scope: !176)
!179 = !DILocation(line: 56, column: 16, scope: !176)
!180 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 23, type: !132, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!181 = !DILocalVariable(name: "s", arg: 1, scope: !180, file: !1, line: 23, type: !135)
!182 = !DILocation(line: 0, scope: !180)
!183 = !DILocation(line: 23, column: 16, scope: !180)
!184 = !DILocation(line: 23, column: 47, scope: !180)
!185 = !DILocation(line: 23, column: 32, scope: !180)
!186 = !DILocation(line: 23, column: 29, scope: !180)
!187 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 28, type: !132, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!188 = !DILocalVariable(name: "s", arg: 1, scope: !187, file: !1, line: 28, type: !135)
!189 = !DILocation(line: 0, scope: !187)
!190 = !DILocation(line: 28, column: 31, scope: !187)
!191 = !DILocation(line: 28, column: 16, scope: !187)
!192 = !DILocation(line: 28, column: 50, scope: !187)
!193 = !DILocation(line: 28, column: 35, scope: !187)
!194 = !DILocation(line: 28, column: 33, scope: !187)
!195 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 33, type: !132, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!196 = !DILocalVariable(name: "s", arg: 1, scope: !195, file: !1, line: 33, type: !135)
!197 = !DILocation(line: 0, scope: !195)
!198 = !DILocation(line: 33, column: 31, scope: !195)
!199 = !DILocation(line: 33, column: 16, scope: !195)
!200 = !DILocation(line: 33, column: 50, scope: !195)
!201 = !DILocation(line: 33, column: 35, scope: !195)
!202 = !DILocation(line: 33, column: 33, scope: !195)
!203 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 38, type: !132, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!204 = !DILocalVariable(name: "s", arg: 1, scope: !203, file: !1, line: 38, type: !135)
!205 = !DILocation(line: 0, scope: !203)
!206 = !DILocation(line: 38, column: 31, scope: !203)
!207 = !DILocation(line: 38, column: 16, scope: !203)
!208 = !DILocation(line: 38, column: 50, scope: !203)
!209 = !DILocation(line: 38, column: 35, scope: !203)
!210 = !DILocation(line: 38, column: 33, scope: !203)
!211 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 43, type: !132, scopeLine: 43, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!212 = !DILocalVariable(name: "s", arg: 1, scope: !211, file: !1, line: 43, type: !135)
!213 = !DILocation(line: 0, scope: !211)
!214 = !DILocation(line: 43, column: 31, scope: !211)
!215 = !DILocation(line: 43, column: 16, scope: !211)
!216 = !DILocation(line: 43, column: 51, scope: !211)
!217 = !DILocation(line: 43, column: 53, scope: !211)
!218 = !DILocation(line: 43, column: 36, scope: !211)
!219 = !DILocation(line: 43, column: 33, scope: !211)
!220 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !1, file: !1, line: 48, type: !132, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!221 = !DILocalVariable(name: "s", arg: 1, scope: !220, file: !1, line: 48, type: !135)
!222 = !DILocation(line: 0, scope: !220)
!223 = !DILocation(line: 48, column: 31, scope: !220)
!224 = !DILocation(line: 48, column: 33, scope: !220)
!225 = !DILocation(line: 48, column: 16, scope: !220)
!226 = !DILocation(line: 48, column: 52, scope: !220)
!227 = !DILocation(line: 48, column: 37, scope: !220)
!228 = !DILocation(line: 48, column: 35, scope: !220)
!229 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !1, file: !1, line: 53, type: !132, scopeLine: 53, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!230 = !DILocalVariable(name: "s", arg: 1, scope: !229, file: !1, line: 53, type: !135)
!231 = !DILocation(line: 0, scope: !229)
!232 = !DILocation(line: 53, column: 31, scope: !229)
!233 = !DILocation(line: 53, column: 33, scope: !229)
!234 = !DILocation(line: 53, column: 16, scope: !229)
!235 = !DILocation(line: 53, column: 52, scope: !229)
!236 = !DILocation(line: 53, column: 37, scope: !229)
!237 = !DILocation(line: 53, column: 35, scope: !229)
!238 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !1, file: !1, line: 58, type: !132, scopeLine: 58, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!239 = !DILocalVariable(name: "s", arg: 1, scope: !238, file: !1, line: 58, type: !135)
!240 = !DILocation(line: 0, scope: !238)
!241 = !DILocation(line: 58, column: 31, scope: !238)
!242 = !DILocation(line: 58, column: 33, scope: !238)
!243 = !DILocation(line: 58, column: 16, scope: !238)
!244 = !DILocation(line: 58, column: 52, scope: !238)
!245 = !DILocation(line: 58, column: 37, scope: !238)
!246 = !DILocation(line: 58, column: 35, scope: !238)
