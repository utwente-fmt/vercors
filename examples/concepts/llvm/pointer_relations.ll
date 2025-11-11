; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/c/pointer_relations.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.A = type { %struct.B, i8, i32, float }
%struct.B = type { i32, float, i8 }

@llvm.used = appending global [16 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6, ptr @PALLAS_SPEC_7, ptr @PALLAS_SPEC_8, ptr @PALLAS_SPEC_12, ptr @PALLAS_SPEC_9, ptr @PALLAS_SPEC_10, ptr @PALLAS_SPEC_11, ptr @PALLAS_SPEC_13, ptr @PALLAS_SPEC_14, ptr @PALLAS_SPEC_15], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @test1(ptr noundef %0) #0 !dbg !17 !pallas.fcontract !36 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !41, metadata !DIExpression()), !dbg !42
  ret void, !dbg !43, !pallas.stmntBlock !44
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @test2(ptr noundef %0) #0 !dbg !48 !pallas.fcontract !49 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !53, metadata !DIExpression()), !dbg !54
  ret void, !dbg !55, !pallas.stmntBlock !56
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test3(ptr noundef %0) #0 !dbg !60 !pallas.fcontract !61 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !65, metadata !DIExpression()), !dbg !66
  ret void, !dbg !67, !pallas.stmntBlock !68
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test4(ptr noundef %0) #0 !dbg !72 !pallas.fcontract !73 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !77, metadata !DIExpression()), !dbg !78
  ret void, !dbg !79, !pallas.stmntBlock !80
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test5(ptr noundef %0) #0 !dbg !84 !pallas.fcontract !85 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !89, metadata !DIExpression()), !dbg !90
  ret void, !dbg !91, !pallas.stmntBlock !92
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test6(ptr noundef %0) #0 !dbg !96 !pallas.fcontract !97 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !101, metadata !DIExpression()), !dbg !102
  ret void, !dbg !103, !pallas.stmntBlock !104
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test7(ptr noundef %0) #0 !dbg !108 !pallas.fcontract !109 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !113, metadata !DIExpression()), !dbg !114
  ret void, !dbg !115, !pallas.stmntBlock !116
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test8(ptr noundef %0) #0 !dbg !120 !pallas.fcontract !121 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !125, metadata !DIExpression()), !dbg !126
  ret void, !dbg !127, !pallas.stmntBlock !128
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !132 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !150, metadata !DIExpression()), !dbg !151
  %2 = icmp ne ptr %0, null, !dbg !152
  ret i1 %2, !dbg !151
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !153 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !154, metadata !DIExpression()), !dbg !155
  %2 = icmp ne ptr %0, null, !dbg !156
  ret i1 %2, !dbg !155
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !157 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !158, metadata !DIExpression()), !dbg !159
  %2 = icmp ne ptr %0, null, !dbg !160
  ret i1 %2, !dbg !159
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !161 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !162, metadata !DIExpression()), !dbg !163
  %2 = icmp ne ptr %0, null, !dbg !164
  ret i1 %2, !dbg !163
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0) #0 !dbg !165 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !166, metadata !DIExpression()), !dbg !167
  %2 = icmp ne ptr %0, null, !dbg !168
  ret i1 %2, !dbg !167
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0) #0 !dbg !169 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !170, metadata !DIExpression()), !dbg !171
  %2 = icmp ne ptr %0, null, !dbg !172
  ret i1 %2, !dbg !171
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0) #0 !dbg !173 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !174, metadata !DIExpression()), !dbg !175
  %2 = icmp ne ptr %0, null, !dbg !176
  ret i1 %2, !dbg !175
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0) #0 !dbg !177 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !178, metadata !DIExpression()), !dbg !179
  %2 = icmp ne ptr %0, null, !dbg !180
  ret i1 %2, !dbg !179
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0) #0 !dbg !181 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !182, metadata !DIExpression()), !dbg !183
  %2 = ptrtoint ptr %0 to i64, !dbg !184
  %3 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !185
  %4 = ptrtoint ptr %3 to i64, !dbg !186
  %5 = icmp eq i64 %2, %4, !dbg !187
  ret i1 %5, !dbg !183
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0) #0 !dbg !188 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !189, metadata !DIExpression()), !dbg !190
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !191
  %3 = ptrtoint ptr %2 to i64, !dbg !192
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !193
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0, !dbg !194
  %6 = ptrtoint ptr %5 to i64, !dbg !195
  %7 = icmp eq i64 %3, %6, !dbg !196
  ret i1 %7, !dbg !190
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0) #0 !dbg !197 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !198, metadata !DIExpression()), !dbg !199
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !200
  %3 = ptrtoint ptr %2 to i64, !dbg !201
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 1, !dbg !202
  %5 = ptrtoint ptr %4 to i64, !dbg !203
  %6 = icmp ult i64 %3, %5, !dbg !204
  ret i1 %6, !dbg !199
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0) #0 !dbg !205 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !206, metadata !DIExpression()), !dbg !207
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 1, !dbg !208
  %3 = ptrtoint ptr %2 to i64, !dbg !209
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 2, !dbg !210
  %5 = ptrtoint ptr %4 to i64, !dbg !211
  %6 = icmp ult i64 %3, %5, !dbg !212
  ret i1 %6, !dbg !207
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0) #0 !dbg !213 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !214, metadata !DIExpression()), !dbg !215
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 2, !dbg !216
  %3 = ptrtoint ptr %2 to i64, !dbg !217
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 3, !dbg !218
  %5 = ptrtoint ptr %4 to i64, !dbg !219
  %6 = icmp ult i64 %3, %5, !dbg !220
  ret i1 %6, !dbg !215
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0) #0 !dbg !221 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !222, metadata !DIExpression()), !dbg !223
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !224
  %3 = getelementptr inbounds %struct.B, ptr %2, i32 0, i32 0, !dbg !225
  %4 = ptrtoint ptr %3 to i64, !dbg !226
  %5 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 1, !dbg !227
  %6 = ptrtoint ptr %5 to i64, !dbg !228
  %7 = icmp ult i64 %4, %6, !dbg !229
  ret i1 %7, !dbg !223
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0) #0 !dbg !230 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !231, metadata !DIExpression()), !dbg !232
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !233
  %3 = getelementptr inbounds %struct.B, ptr %2, i32 0, i32 1, !dbg !234
  %4 = ptrtoint ptr %3 to i64, !dbg !235
  %5 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 2, !dbg !236
  %6 = ptrtoint ptr %5 to i64, !dbg !237
  %7 = icmp ult i64 %4, %6, !dbg !238
  ret i1 %7, !dbg !232
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0) #0 !dbg !239 !pallas.exprWrapper !149 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !240, metadata !DIExpression()), !dbg !241
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !242
  %3 = getelementptr inbounds %struct.B, ptr %2, i32 0, i32 2, !dbg !243
  %4 = ptrtoint ptr %3 to i64, !dbg !244
  %5 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 3, !dbg !245
  %6 = ptrtoint ptr %5 to i64, !dbg !246
  %7 = icmp ult i64 %4, %6, !dbg !247
  ret i1 %7, !dbg !241
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!9, !10, !11, !12, !13, !14, !15}
!llvm.ident = !{!16, !16}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/c/pointer_relations.c", directory: ".", checksumkind: CSK_MD5, checksum: "6d634623b3efb2fa906e17a0980f974d")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !4, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "5967dc9ce63807dabdd0aa72ebf4cd4d")
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
!36 = !{!37, i1 false, i1 false, !39}
!37 = !{!"pallas.srcLoc", i64 21, i64 1, i64 21, i64 24, !38}
!38 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/c/pointer_relations.c", directory: "", checksumkind: CSK_MD5, checksum: "6d634623b3efb2fa906e17a0980f974d")
!39 = !{!"pallas.requires", !40, ptr @PALLAS_SPEC_0, !41}
!40 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 23, !38}
!41 = !DILocalVariable(name: "s", arg: 1, scope: !17, file: !1, line: 22, type: !20)
!42 = !DILocation(line: 22, column: 22, scope: !17)
!43 = !DILocation(line: 24, column: 1, scope: !17)
!44 = !{!45, !46}
!45 = !{!"pallas.srcLoc", i64 23, i64 5, i64 23, i64 49, !38}
!46 = !{!"pallas.assert", !47, ptr @PALLAS_SPEC_8, !41}
!47 = !{!"pallas.srcLoc", i64 23, i64 9, i64 23, i64 48, !38}
!48 = distinct !DISubprogram(name: "test2", scope: !1, file: !1, line: 27, type: !18, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!49 = !{!50, i1 false, i1 false, !51}
!50 = !{!"pallas.srcLoc", i64 26, i64 1, i64 26, i64 24, !38}
!51 = !{!"pallas.requires", !52, ptr @PALLAS_SPEC_1, !53}
!52 = !{!"pallas.srcLoc", i64 26, i64 5, i64 26, i64 23, !38}
!53 = !DILocalVariable(name: "s", arg: 1, scope: !48, file: !1, line: 27, type: !20)
!54 = !DILocation(line: 27, column: 22, scope: !48)
!55 = !DILocation(line: 29, column: 1, scope: !48)
!56 = !{!57, !58}
!57 = !{!"pallas.srcLoc", i64 28, i64 5, i64 28, i64 53, !38}
!58 = !{!"pallas.assert", !59, ptr @PALLAS_SPEC_9, !53}
!59 = !{!"pallas.srcLoc", i64 28, i64 9, i64 28, i64 51, !38}
!60 = distinct !DISubprogram(name: "test3", scope: !1, file: !1, line: 32, type: !18, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!61 = !{!62, i1 false, i1 false, !63}
!62 = !{!"pallas.srcLoc", i64 31, i64 1, i64 31, i64 24, !38}
!63 = !{!"pallas.requires", !64, ptr @PALLAS_SPEC_2, !65}
!64 = !{!"pallas.srcLoc", i64 31, i64 5, i64 31, i64 23, !38}
!65 = !DILocalVariable(name: "s", arg: 1, scope: !60, file: !1, line: 32, type: !20)
!66 = !DILocation(line: 32, column: 22, scope: !60)
!67 = !DILocation(line: 34, column: 1, scope: !60)
!68 = !{!69, !70}
!69 = !{!"pallas.srcLoc", i64 33, i64 5, i64 33, i64 53, !38}
!70 = !{!"pallas.assert", !71, ptr @PALLAS_SPEC_10, !65}
!71 = !{!"pallas.srcLoc", i64 33, i64 9, i64 33, i64 51, !38}
!72 = distinct !DISubprogram(name: "test4", scope: !1, file: !1, line: 37, type: !18, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!73 = !{!74, i1 false, i1 false, !75}
!74 = !{!"pallas.srcLoc", i64 36, i64 1, i64 36, i64 24, !38}
!75 = !{!"pallas.requires", !76, ptr @PALLAS_SPEC_3, !77}
!76 = !{!"pallas.srcLoc", i64 36, i64 5, i64 36, i64 23, !38}
!77 = !DILocalVariable(name: "s", arg: 1, scope: !72, file: !1, line: 37, type: !20)
!78 = !DILocation(line: 37, column: 22, scope: !72)
!79 = !DILocation(line: 39, column: 1, scope: !72)
!80 = !{!81, !82}
!81 = !{!"pallas.srcLoc", i64 38, i64 5, i64 38, i64 53, !38}
!82 = !{!"pallas.assert", !83, ptr @PALLAS_SPEC_11, !77}
!83 = !{!"pallas.srcLoc", i64 38, i64 9, i64 38, i64 51, !38}
!84 = distinct !DISubprogram(name: "test5", scope: !1, file: !1, line: 42, type: !18, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!85 = !{!86, i1 false, i1 false, !87}
!86 = !{!"pallas.srcLoc", i64 41, i64 1, i64 41, i64 24, !38}
!87 = !{!"pallas.requires", !88, ptr @PALLAS_SPEC_4, !89}
!88 = !{!"pallas.srcLoc", i64 41, i64 5, i64 41, i64 23, !38}
!89 = !DILocalVariable(name: "s", arg: 1, scope: !84, file: !1, line: 42, type: !20)
!90 = !DILocation(line: 42, column: 22, scope: !84)
!91 = !DILocation(line: 44, column: 1, scope: !84)
!92 = !{!93, !94}
!93 = !{!"pallas.srcLoc", i64 43, i64 5, i64 43, i64 56, !38}
!94 = !{!"pallas.assert", !95, ptr @PALLAS_SPEC_12, !89}
!95 = !{!"pallas.srcLoc", i64 43, i64 9, i64 43, i64 54, !38}
!96 = distinct !DISubprogram(name: "test6", scope: !1, file: !1, line: 47, type: !18, scopeLine: 47, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!97 = !{!98, i1 false, i1 false, !99}
!98 = !{!"pallas.srcLoc", i64 46, i64 1, i64 46, i64 24, !38}
!99 = !{!"pallas.requires", !100, ptr @PALLAS_SPEC_5, !101}
!100 = !{!"pallas.srcLoc", i64 46, i64 5, i64 46, i64 23, !38}
!101 = !DILocalVariable(name: "s", arg: 1, scope: !96, file: !1, line: 47, type: !20)
!102 = !DILocation(line: 47, column: 22, scope: !96)
!103 = !DILocation(line: 49, column: 1, scope: !96)
!104 = !{!105, !106}
!105 = !{!"pallas.srcLoc", i64 48, i64 5, i64 48, i64 55, !38}
!106 = !{!"pallas.assert", !107, ptr @PALLAS_SPEC_13, !101}
!107 = !{!"pallas.srcLoc", i64 48, i64 9, i64 48, i64 53, !38}
!108 = distinct !DISubprogram(name: "test7", scope: !1, file: !1, line: 52, type: !18, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!109 = !{!110, i1 false, i1 false, !111}
!110 = !{!"pallas.srcLoc", i64 51, i64 1, i64 51, i64 24, !38}
!111 = !{!"pallas.requires", !112, ptr @PALLAS_SPEC_6, !113}
!112 = !{!"pallas.srcLoc", i64 51, i64 5, i64 51, i64 23, !38}
!113 = !DILocalVariable(name: "s", arg: 1, scope: !108, file: !1, line: 52, type: !20)
!114 = !DILocation(line: 52, column: 22, scope: !108)
!115 = !DILocation(line: 54, column: 1, scope: !108)
!116 = !{!117, !118}
!117 = !{!"pallas.srcLoc", i64 53, i64 5, i64 53, i64 55, !38}
!118 = !{!"pallas.assert", !119, ptr @PALLAS_SPEC_14, !113}
!119 = !{!"pallas.srcLoc", i64 53, i64 9, i64 53, i64 53, !38}
!120 = distinct !DISubprogram(name: "test8", scope: !1, file: !1, line: 57, type: !18, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!121 = !{!122, i1 false, i1 false, !123}
!122 = !{!"pallas.srcLoc", i64 56, i64 1, i64 56, i64 24, !38}
!123 = !{!"pallas.requires", !124, ptr @PALLAS_SPEC_7, !125}
!124 = !{!"pallas.srcLoc", i64 56, i64 5, i64 56, i64 23, !38}
!125 = !DILocalVariable(name: "s", arg: 1, scope: !120, file: !1, line: 57, type: !20)
!126 = !DILocation(line: 57, column: 22, scope: !120)
!127 = !DILocation(line: 59, column: 1, scope: !120)
!128 = !{!129, !130}
!129 = !{!"pallas.srcLoc", i64 58, i64 5, i64 58, i64 55, !38}
!130 = !{!"pallas.assert", !131, ptr @PALLAS_SPEC_15, !125}
!131 = !{!"pallas.srcLoc", i64 58, i64 9, i64 58, i64 53, !38}
!132 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 21, type: !133, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!133 = !DISubroutineType(types: !134)
!134 = !{!135, !136}
!135 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!136 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !137, size: 64)
!137 = !DIDerivedType(tag: DW_TAG_typedef, name: "A", file: !3, line: 19, baseType: !138)
!138 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !3, line: 12, size: 192, elements: !139)
!139 = !{!140, !146, !147, !148}
!140 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !138, file: !3, line: 13, baseType: !141, size: 96)
!141 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "B", file: !3, line: 4, size: 96, elements: !142)
!142 = !{!143, !144, !145}
!143 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !141, file: !3, line: 5, baseType: !27, size: 32)
!144 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !141, file: !3, line: 6, baseType: !29, size: 32, offset: 32)
!145 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !141, file: !3, line: 7, baseType: !31, size: 8, offset: 64)
!146 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !138, file: !3, line: 14, baseType: !31, size: 8, offset: 96)
!147 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !138, file: !3, line: 15, baseType: !27, size: 32, offset: 128)
!148 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !138, file: !3, line: 16, baseType: !29, size: 32, offset: 160)
!149 = !{!""}
!150 = !DILocalVariable(name: "s", arg: 1, scope: !132, file: !1, line: 21, type: !136)
!151 = !DILocation(line: 0, scope: !132)
!152 = !DILocation(line: 21, column: 16, scope: !132)
!153 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 26, type: !133, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!154 = !DILocalVariable(name: "s", arg: 1, scope: !153, file: !1, line: 26, type: !136)
!155 = !DILocation(line: 0, scope: !153)
!156 = !DILocation(line: 26, column: 16, scope: !153)
!157 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 31, type: !133, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!158 = !DILocalVariable(name: "s", arg: 1, scope: !157, file: !1, line: 31, type: !136)
!159 = !DILocation(line: 0, scope: !157)
!160 = !DILocation(line: 31, column: 16, scope: !157)
!161 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 36, type: !133, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!162 = !DILocalVariable(name: "s", arg: 1, scope: !161, file: !1, line: 36, type: !136)
!163 = !DILocation(line: 0, scope: !161)
!164 = !DILocation(line: 36, column: 16, scope: !161)
!165 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 41, type: !133, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!166 = !DILocalVariable(name: "s", arg: 1, scope: !165, file: !1, line: 41, type: !136)
!167 = !DILocation(line: 0, scope: !165)
!168 = !DILocation(line: 41, column: 16, scope: !165)
!169 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 46, type: !133, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!170 = !DILocalVariable(name: "s", arg: 1, scope: !169, file: !1, line: 46, type: !136)
!171 = !DILocation(line: 0, scope: !169)
!172 = !DILocation(line: 46, column: 16, scope: !169)
!173 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 51, type: !133, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!174 = !DILocalVariable(name: "s", arg: 1, scope: !173, file: !1, line: 51, type: !136)
!175 = !DILocation(line: 0, scope: !173)
!176 = !DILocation(line: 51, column: 16, scope: !173)
!177 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 56, type: !133, scopeLine: 56, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!178 = !DILocalVariable(name: "s", arg: 1, scope: !177, file: !1, line: 56, type: !136)
!179 = !DILocation(line: 0, scope: !177)
!180 = !DILocation(line: 56, column: 16, scope: !177)
!181 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 23, type: !133, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!182 = !DILocalVariable(name: "s", arg: 1, scope: !181, file: !1, line: 23, type: !136)
!183 = !DILocation(line: 0, scope: !181)
!184 = !DILocation(line: 23, column: 16, scope: !181)
!185 = !DILocation(line: 23, column: 47, scope: !181)
!186 = !DILocation(line: 23, column: 32, scope: !181)
!187 = !DILocation(line: 23, column: 29, scope: !181)
!188 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 43, type: !133, scopeLine: 43, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!189 = !DILocalVariable(name: "s", arg: 1, scope: !188, file: !1, line: 43, type: !136)
!190 = !DILocation(line: 0, scope: !188)
!191 = !DILocation(line: 43, column: 31, scope: !188)
!192 = !DILocation(line: 43, column: 16, scope: !188)
!193 = !DILocation(line: 43, column: 51, scope: !188)
!194 = !DILocation(line: 43, column: 53, scope: !188)
!195 = !DILocation(line: 43, column: 36, scope: !188)
!196 = !DILocation(line: 43, column: 33, scope: !188)
!197 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 28, type: !133, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!198 = !DILocalVariable(name: "s", arg: 1, scope: !197, file: !1, line: 28, type: !136)
!199 = !DILocation(line: 0, scope: !197)
!200 = !DILocation(line: 28, column: 31, scope: !197)
!201 = !DILocation(line: 28, column: 16, scope: !197)
!202 = !DILocation(line: 28, column: 50, scope: !197)
!203 = !DILocation(line: 28, column: 35, scope: !197)
!204 = !DILocation(line: 28, column: 33, scope: !197)
!205 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 33, type: !133, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!206 = !DILocalVariable(name: "s", arg: 1, scope: !205, file: !1, line: 33, type: !136)
!207 = !DILocation(line: 0, scope: !205)
!208 = !DILocation(line: 33, column: 31, scope: !205)
!209 = !DILocation(line: 33, column: 16, scope: !205)
!210 = !DILocation(line: 33, column: 50, scope: !205)
!211 = !DILocation(line: 33, column: 35, scope: !205)
!212 = !DILocation(line: 33, column: 33, scope: !205)
!213 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 38, type: !133, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!214 = !DILocalVariable(name: "s", arg: 1, scope: !213, file: !1, line: 38, type: !136)
!215 = !DILocation(line: 0, scope: !213)
!216 = !DILocation(line: 38, column: 31, scope: !213)
!217 = !DILocation(line: 38, column: 16, scope: !213)
!218 = !DILocation(line: 38, column: 50, scope: !213)
!219 = !DILocation(line: 38, column: 35, scope: !213)
!220 = !DILocation(line: 38, column: 33, scope: !213)
!221 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !1, file: !1, line: 48, type: !133, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!222 = !DILocalVariable(name: "s", arg: 1, scope: !221, file: !1, line: 48, type: !136)
!223 = !DILocation(line: 0, scope: !221)
!224 = !DILocation(line: 48, column: 31, scope: !221)
!225 = !DILocation(line: 48, column: 33, scope: !221)
!226 = !DILocation(line: 48, column: 16, scope: !221)
!227 = !DILocation(line: 48, column: 52, scope: !221)
!228 = !DILocation(line: 48, column: 37, scope: !221)
!229 = !DILocation(line: 48, column: 35, scope: !221)
!230 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !1, file: !1, line: 53, type: !133, scopeLine: 53, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!231 = !DILocalVariable(name: "s", arg: 1, scope: !230, file: !1, line: 53, type: !136)
!232 = !DILocation(line: 0, scope: !230)
!233 = !DILocation(line: 53, column: 31, scope: !230)
!234 = !DILocation(line: 53, column: 33, scope: !230)
!235 = !DILocation(line: 53, column: 16, scope: !230)
!236 = !DILocation(line: 53, column: 52, scope: !230)
!237 = !DILocation(line: 53, column: 37, scope: !230)
!238 = !DILocation(line: 53, column: 35, scope: !230)
!239 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !1, file: !1, line: 58, type: !133, scopeLine: 58, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!240 = !DILocalVariable(name: "s", arg: 1, scope: !239, file: !1, line: 58, type: !136)
!241 = !DILocation(line: 0, scope: !239)
!242 = !DILocation(line: 58, column: 31, scope: !239)
!243 = !DILocation(line: 58, column: 33, scope: !239)
!244 = !DILocation(line: 58, column: 16, scope: !239)
!245 = !DILocation(line: 58, column: 52, scope: !239)
!246 = !DILocation(line: 58, column: 37, scope: !239)
!247 = !DILocation(line: 58, column: 35, scope: !239)
