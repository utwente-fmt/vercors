; ModuleID = 'tmp/tmp_ir_source0.ll'
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
  call void @llvm.dbg.declare(metadata ptr %2, metadata !43, metadata !DIExpression()), !dbg !62
  ret void, !dbg !63, !pallas.stmntBlock !64
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local void @test2(ptr noundef %0) #0 !dbg !72 !pallas.fcontract !73 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !79, metadata !DIExpression()), !dbg !82
  ret void, !dbg !83, !pallas.stmntBlock !84
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test3(ptr noundef %0) #0 !dbg !92 !pallas.fcontract !93 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !99, metadata !DIExpression()), !dbg !102
  ret void, !dbg !103, !pallas.stmntBlock !104
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test4(ptr noundef %0) #0 !dbg !112 !pallas.fcontract !113 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !119, metadata !DIExpression()), !dbg !122
  ret void, !dbg !123, !pallas.stmntBlock !124
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test5(ptr noundef %0) #0 !dbg !132 !pallas.fcontract !133 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !139, metadata !DIExpression()), !dbg !142
  ret void, !dbg !143, !pallas.stmntBlock !144
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test6(ptr noundef %0) #0 !dbg !152 !pallas.fcontract !153 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !159, metadata !DIExpression()), !dbg !162
  ret void, !dbg !163, !pallas.stmntBlock !164
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test7(ptr noundef %0) #0 !dbg !172 !pallas.fcontract !173 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !179, metadata !DIExpression()), !dbg !182
  ret void, !dbg !183, !pallas.stmntBlock !184
}

; Function Attrs: noinline nounwind uwtable
define dso_local void @test8(ptr noundef %0) #0 !dbg !192 !pallas.fcontract !193 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !199, metadata !DIExpression()), !dbg !202
  ret void, !dbg !203, !pallas.stmntBlock !204
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef %0) #0 !dbg !45 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !44, metadata !DIExpression()), !dbg !213
  %2 = icmp ne ptr %0, null, !dbg !214
  ret i1 %2, !dbg !213
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef %0) #0 !dbg !81 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !80, metadata !DIExpression()), !dbg !215
  %2 = icmp ne ptr %0, null, !dbg !216
  ret i1 %2, !dbg !215
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef %0) #0 !dbg !101 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !100, metadata !DIExpression()), !dbg !217
  %2 = icmp ne ptr %0, null, !dbg !218
  ret i1 %2, !dbg !217
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef %0) #0 !dbg !121 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !120, metadata !DIExpression()), !dbg !219
  %2 = icmp ne ptr %0, null, !dbg !220
  ret i1 %2, !dbg !219
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4(ptr noundef %0) #0 !dbg !141 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !140, metadata !DIExpression()), !dbg !221
  %2 = icmp ne ptr %0, null, !dbg !222
  ret i1 %2, !dbg !221
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef %0) #0 !dbg !161 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !160, metadata !DIExpression()), !dbg !223
  %2 = icmp ne ptr %0, null, !dbg !224
  ret i1 %2, !dbg !223
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef %0) #0 !dbg !181 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !180, metadata !DIExpression()), !dbg !225
  %2 = icmp ne ptr %0, null, !dbg !226
  ret i1 %2, !dbg !225
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_7(ptr noundef %0) #0 !dbg !201 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !200, metadata !DIExpression()), !dbg !227
  %2 = icmp ne ptr %0, null, !dbg !228
  ret i1 %2, !dbg !227
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_8(ptr noundef %0) #0 !dbg !71 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !70, metadata !DIExpression()), !dbg !229
  %2 = ptrtoint ptr %0 to i64, !dbg !230
  %3 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !231
  %4 = ptrtoint ptr %3 to i64, !dbg !232
  %5 = icmp eq i64 %2, %4, !dbg !233
  ret i1 %5, !dbg !229
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_9(ptr noundef %0) #0 !dbg !91 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !90, metadata !DIExpression()), !dbg !234
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !235
  %3 = ptrtoint ptr %2 to i64, !dbg !236
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 1, !dbg !237
  %5 = ptrtoint ptr %4 to i64, !dbg !238
  %6 = icmp ult i64 %3, %5, !dbg !239
  ret i1 %6, !dbg !234
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_10(ptr noundef %0) #0 !dbg !111 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !110, metadata !DIExpression()), !dbg !240
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 1, !dbg !241
  %3 = ptrtoint ptr %2 to i64, !dbg !242
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 2, !dbg !243
  %5 = ptrtoint ptr %4 to i64, !dbg !244
  %6 = icmp ult i64 %3, %5, !dbg !245
  ret i1 %6, !dbg !240
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_11(ptr noundef %0) #0 !dbg !131 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !130, metadata !DIExpression()), !dbg !246
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 2, !dbg !247
  %3 = ptrtoint ptr %2 to i64, !dbg !248
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 3, !dbg !249
  %5 = ptrtoint ptr %4 to i64, !dbg !250
  %6 = icmp ult i64 %3, %5, !dbg !251
  ret i1 %6, !dbg !246
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_12(ptr noundef %0) #0 !dbg !151 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !150, metadata !DIExpression()), !dbg !252
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !253
  %3 = ptrtoint ptr %2 to i64, !dbg !254
  %4 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !255
  %5 = getelementptr inbounds %struct.B, ptr %4, i32 0, i32 0, !dbg !256
  %6 = ptrtoint ptr %5 to i64, !dbg !257
  %7 = icmp eq i64 %3, %6, !dbg !258
  ret i1 %7, !dbg !252
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_13(ptr noundef %0) #0 !dbg !171 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !170, metadata !DIExpression()), !dbg !259
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !260
  %3 = getelementptr inbounds %struct.B, ptr %2, i32 0, i32 0, !dbg !261
  %4 = ptrtoint ptr %3 to i64, !dbg !262
  %5 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 1, !dbg !263
  %6 = ptrtoint ptr %5 to i64, !dbg !264
  %7 = icmp ult i64 %4, %6, !dbg !265
  ret i1 %7, !dbg !259
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_14(ptr noundef %0) #0 !dbg !191 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !190, metadata !DIExpression()), !dbg !266
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !267
  %3 = getelementptr inbounds %struct.B, ptr %2, i32 0, i32 1, !dbg !268
  %4 = ptrtoint ptr %3 to i64, !dbg !269
  %5 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 2, !dbg !270
  %6 = ptrtoint ptr %5 to i64, !dbg !271
  %7 = icmp ult i64 %4, %6, !dbg !272
  ret i1 %7, !dbg !266
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_15(ptr noundef %0) #0 !dbg !211 !pallas.exprWrapper !212 {
  call void @llvm.dbg.value(metadata ptr %0, metadata !210, metadata !DIExpression()), !dbg !273
  %2 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 0, !dbg !274
  %3 = getelementptr inbounds %struct.B, ptr %2, i32 0, i32 2, !dbg !275
  %4 = ptrtoint ptr %3 to i64, !dbg !276
  %5 = getelementptr inbounds %struct.A, ptr %0, i32 0, i32 3, !dbg !277
  %6 = ptrtoint ptr %5 to i64, !dbg !278
  %7 = icmp ult i64 %4, %6, !dbg !279
  ret i1 %7, !dbg !273
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
!36 = !{!37, i1 false, i1 false, !35, !35, !39}
!37 = !{!"pallas.srcLoc", i64 21, i64 1, i64 21, i64 24, !38}
!38 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/c/pointer_relations.c", directory: "", checksumkind: CSK_MD5, checksum: "6d634623b3efb2fa906e17a0980f974d")
!39 = !{!"pallas.requires", !40, ptr @PALLAS_SPEC_0, !35, !35, !41}
!40 = !{!"pallas.srcLoc", i64 21, i64 5, i64 21, i64 23, !38}
!41 = !{!42}
!42 = !{!43, !44}
!43 = !DILocalVariable(name: "s", arg: 1, scope: !17, file: !1, line: 22, type: !20)
!44 = !DILocalVariable(name: "s", arg: 1, scope: !45, file: !1, line: 21, type: !49)
!45 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 21, type: !46, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!46 = !DISubroutineType(types: !47)
!47 = !{!48, !49}
!48 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!49 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !50, size: 64)
!50 = !DIDerivedType(tag: DW_TAG_typedef, name: "A", file: !3, line: 19, baseType: !51)
!51 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "A", file: !3, line: 12, size: 192, elements: !52)
!52 = !{!53, !59, !60, !61}
!53 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !51, file: !3, line: 13, baseType: !54, size: 96)
!54 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "B", file: !3, line: 4, size: 96, elements: !55)
!55 = !{!56, !57, !58}
!56 = !DIDerivedType(tag: DW_TAG_member, name: "e", scope: !54, file: !3, line: 5, baseType: !27, size: 32)
!57 = !DIDerivedType(tag: DW_TAG_member, name: "f", scope: !54, file: !3, line: 6, baseType: !29, size: 32, offset: 32)
!58 = !DIDerivedType(tag: DW_TAG_member, name: "g", scope: !54, file: !3, line: 7, baseType: !31, size: 8, offset: 64)
!59 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !51, file: !3, line: 14, baseType: !31, size: 8, offset: 96)
!60 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !51, file: !3, line: 15, baseType: !27, size: 32, offset: 128)
!61 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !51, file: !3, line: 16, baseType: !29, size: 32, offset: 160)
!62 = !DILocation(line: 22, column: 22, scope: !17)
!63 = !DILocation(line: 24, column: 1, scope: !17)
!64 = !{!65, !66}
!65 = !{!"pallas.srcLoc", i64 23, i64 5, i64 23, i64 49, !38}
!66 = !{!"pallas.assert", !67, ptr @PALLAS_SPEC_8, !35, !35, !68}
!67 = !{!"pallas.srcLoc", i64 23, i64 9, i64 23, i64 48, !38}
!68 = !{!69}
!69 = !{!43, !70}
!70 = !DILocalVariable(name: "s", arg: 1, scope: !71, file: !1, line: 23, type: !49)
!71 = distinct !DISubprogram(name: "PALLAS_SPEC_8", scope: !1, file: !1, line: 23, type: !46, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!72 = distinct !DISubprogram(name: "test2", scope: !1, file: !1, line: 27, type: !18, scopeLine: 27, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!73 = !{!74, i1 false, i1 false, !35, !35, !75}
!74 = !{!"pallas.srcLoc", i64 26, i64 1, i64 26, i64 24, !38}
!75 = !{!"pallas.requires", !76, ptr @PALLAS_SPEC_1, !35, !35, !77}
!76 = !{!"pallas.srcLoc", i64 26, i64 5, i64 26, i64 23, !38}
!77 = !{!78}
!78 = !{!79, !80}
!79 = !DILocalVariable(name: "s", arg: 1, scope: !72, file: !1, line: 27, type: !20)
!80 = !DILocalVariable(name: "s", arg: 1, scope: !81, file: !1, line: 26, type: !49)
!81 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 26, type: !46, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!82 = !DILocation(line: 27, column: 22, scope: !72)
!83 = !DILocation(line: 29, column: 1, scope: !72)
!84 = !{!85, !86}
!85 = !{!"pallas.srcLoc", i64 28, i64 5, i64 28, i64 53, !38}
!86 = !{!"pallas.assert", !87, ptr @PALLAS_SPEC_9, !35, !35, !88}
!87 = !{!"pallas.srcLoc", i64 28, i64 9, i64 28, i64 51, !38}
!88 = !{!89}
!89 = !{!79, !90}
!90 = !DILocalVariable(name: "s", arg: 1, scope: !91, file: !1, line: 28, type: !49)
!91 = distinct !DISubprogram(name: "PALLAS_SPEC_9", scope: !1, file: !1, line: 28, type: !46, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!92 = distinct !DISubprogram(name: "test3", scope: !1, file: !1, line: 32, type: !18, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!93 = !{!94, i1 false, i1 false, !35, !35, !95}
!94 = !{!"pallas.srcLoc", i64 31, i64 1, i64 31, i64 24, !38}
!95 = !{!"pallas.requires", !96, ptr @PALLAS_SPEC_2, !35, !35, !97}
!96 = !{!"pallas.srcLoc", i64 31, i64 5, i64 31, i64 23, !38}
!97 = !{!98}
!98 = !{!99, !100}
!99 = !DILocalVariable(name: "s", arg: 1, scope: !92, file: !1, line: 32, type: !20)
!100 = !DILocalVariable(name: "s", arg: 1, scope: !101, file: !1, line: 31, type: !49)
!101 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 31, type: !46, scopeLine: 31, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!102 = !DILocation(line: 32, column: 22, scope: !92)
!103 = !DILocation(line: 34, column: 1, scope: !92)
!104 = !{!105, !106}
!105 = !{!"pallas.srcLoc", i64 33, i64 5, i64 33, i64 53, !38}
!106 = !{!"pallas.assert", !107, ptr @PALLAS_SPEC_10, !35, !35, !108}
!107 = !{!"pallas.srcLoc", i64 33, i64 9, i64 33, i64 51, !38}
!108 = !{!109}
!109 = !{!99, !110}
!110 = !DILocalVariable(name: "s", arg: 1, scope: !111, file: !1, line: 33, type: !49)
!111 = distinct !DISubprogram(name: "PALLAS_SPEC_10", scope: !1, file: !1, line: 33, type: !46, scopeLine: 33, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!112 = distinct !DISubprogram(name: "test4", scope: !1, file: !1, line: 37, type: !18, scopeLine: 37, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!113 = !{!114, i1 false, i1 false, !35, !35, !115}
!114 = !{!"pallas.srcLoc", i64 36, i64 1, i64 36, i64 24, !38}
!115 = !{!"pallas.requires", !116, ptr @PALLAS_SPEC_3, !35, !35, !117}
!116 = !{!"pallas.srcLoc", i64 36, i64 5, i64 36, i64 23, !38}
!117 = !{!118}
!118 = !{!119, !120}
!119 = !DILocalVariable(name: "s", arg: 1, scope: !112, file: !1, line: 37, type: !20)
!120 = !DILocalVariable(name: "s", arg: 1, scope: !121, file: !1, line: 36, type: !49)
!121 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 36, type: !46, scopeLine: 36, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!122 = !DILocation(line: 37, column: 22, scope: !112)
!123 = !DILocation(line: 39, column: 1, scope: !112)
!124 = !{!125, !126}
!125 = !{!"pallas.srcLoc", i64 38, i64 5, i64 38, i64 53, !38}
!126 = !{!"pallas.assert", !127, ptr @PALLAS_SPEC_11, !35, !35, !128}
!127 = !{!"pallas.srcLoc", i64 38, i64 9, i64 38, i64 51, !38}
!128 = !{!129}
!129 = !{!119, !130}
!130 = !DILocalVariable(name: "s", arg: 1, scope: !131, file: !1, line: 38, type: !49)
!131 = distinct !DISubprogram(name: "PALLAS_SPEC_11", scope: !1, file: !1, line: 38, type: !46, scopeLine: 38, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!132 = distinct !DISubprogram(name: "test5", scope: !1, file: !1, line: 42, type: !18, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!133 = !{!134, i1 false, i1 false, !35, !35, !135}
!134 = !{!"pallas.srcLoc", i64 41, i64 1, i64 41, i64 24, !38}
!135 = !{!"pallas.requires", !136, ptr @PALLAS_SPEC_4, !35, !35, !137}
!136 = !{!"pallas.srcLoc", i64 41, i64 5, i64 41, i64 23, !38}
!137 = !{!138}
!138 = !{!139, !140}
!139 = !DILocalVariable(name: "s", arg: 1, scope: !132, file: !1, line: 42, type: !20)
!140 = !DILocalVariable(name: "s", arg: 1, scope: !141, file: !1, line: 41, type: !49)
!141 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 41, type: !46, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!142 = !DILocation(line: 42, column: 22, scope: !132)
!143 = !DILocation(line: 44, column: 1, scope: !132)
!144 = !{!145, !146}
!145 = !{!"pallas.srcLoc", i64 43, i64 5, i64 43, i64 56, !38}
!146 = !{!"pallas.assert", !147, ptr @PALLAS_SPEC_12, !35, !35, !148}
!147 = !{!"pallas.srcLoc", i64 43, i64 9, i64 43, i64 54, !38}
!148 = !{!149}
!149 = !{!139, !150}
!150 = !DILocalVariable(name: "s", arg: 1, scope: !151, file: !1, line: 43, type: !49)
!151 = distinct !DISubprogram(name: "PALLAS_SPEC_12", scope: !1, file: !1, line: 43, type: !46, scopeLine: 43, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!152 = distinct !DISubprogram(name: "test6", scope: !1, file: !1, line: 47, type: !18, scopeLine: 47, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!153 = !{!154, i1 false, i1 false, !35, !35, !155}
!154 = !{!"pallas.srcLoc", i64 46, i64 1, i64 46, i64 24, !38}
!155 = !{!"pallas.requires", !156, ptr @PALLAS_SPEC_5, !35, !35, !157}
!156 = !{!"pallas.srcLoc", i64 46, i64 5, i64 46, i64 23, !38}
!157 = !{!158}
!158 = !{!159, !160}
!159 = !DILocalVariable(name: "s", arg: 1, scope: !152, file: !1, line: 47, type: !20)
!160 = !DILocalVariable(name: "s", arg: 1, scope: !161, file: !1, line: 46, type: !49)
!161 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 46, type: !46, scopeLine: 46, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!162 = !DILocation(line: 47, column: 22, scope: !152)
!163 = !DILocation(line: 49, column: 1, scope: !152)
!164 = !{!165, !166}
!165 = !{!"pallas.srcLoc", i64 48, i64 5, i64 48, i64 55, !38}
!166 = !{!"pallas.assert", !167, ptr @PALLAS_SPEC_13, !35, !35, !168}
!167 = !{!"pallas.srcLoc", i64 48, i64 9, i64 48, i64 53, !38}
!168 = !{!169}
!169 = !{!159, !170}
!170 = !DILocalVariable(name: "s", arg: 1, scope: !171, file: !1, line: 48, type: !49)
!171 = distinct !DISubprogram(name: "PALLAS_SPEC_13", scope: !1, file: !1, line: 48, type: !46, scopeLine: 48, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!172 = distinct !DISubprogram(name: "test7", scope: !1, file: !1, line: 52, type: !18, scopeLine: 52, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!173 = !{!174, i1 false, i1 false, !35, !35, !175}
!174 = !{!"pallas.srcLoc", i64 51, i64 1, i64 51, i64 24, !38}
!175 = !{!"pallas.requires", !176, ptr @PALLAS_SPEC_6, !35, !35, !177}
!176 = !{!"pallas.srcLoc", i64 51, i64 5, i64 51, i64 23, !38}
!177 = !{!178}
!178 = !{!179, !180}
!179 = !DILocalVariable(name: "s", arg: 1, scope: !172, file: !1, line: 52, type: !20)
!180 = !DILocalVariable(name: "s", arg: 1, scope: !181, file: !1, line: 51, type: !49)
!181 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 51, type: !46, scopeLine: 51, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!182 = !DILocation(line: 52, column: 22, scope: !172)
!183 = !DILocation(line: 54, column: 1, scope: !172)
!184 = !{!185, !186}
!185 = !{!"pallas.srcLoc", i64 53, i64 5, i64 53, i64 55, !38}
!186 = !{!"pallas.assert", !187, ptr @PALLAS_SPEC_14, !35, !35, !188}
!187 = !{!"pallas.srcLoc", i64 53, i64 9, i64 53, i64 53, !38}
!188 = !{!189}
!189 = !{!179, !190}
!190 = !DILocalVariable(name: "s", arg: 1, scope: !191, file: !1, line: 53, type: !49)
!191 = distinct !DISubprogram(name: "PALLAS_SPEC_14", scope: !1, file: !1, line: 53, type: !46, scopeLine: 53, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!192 = distinct !DISubprogram(name: "test8", scope: !1, file: !1, line: 57, type: !18, scopeLine: 57, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!193 = !{!194, i1 false, i1 false, !35, !35, !195}
!194 = !{!"pallas.srcLoc", i64 56, i64 1, i64 56, i64 24, !38}
!195 = !{!"pallas.requires", !196, ptr @PALLAS_SPEC_7, !35, !35, !197}
!196 = !{!"pallas.srcLoc", i64 56, i64 5, i64 56, i64 23, !38}
!197 = !{!198}
!198 = !{!199, !200}
!199 = !DILocalVariable(name: "s", arg: 1, scope: !192, file: !1, line: 57, type: !20)
!200 = !DILocalVariable(name: "s", arg: 1, scope: !201, file: !1, line: 56, type: !49)
!201 = distinct !DISubprogram(name: "PALLAS_SPEC_7", scope: !1, file: !1, line: 56, type: !46, scopeLine: 56, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!202 = !DILocation(line: 57, column: 22, scope: !192)
!203 = !DILocation(line: 59, column: 1, scope: !192)
!204 = !{!205, !206}
!205 = !{!"pallas.srcLoc", i64 58, i64 5, i64 58, i64 55, !38}
!206 = !{!"pallas.assert", !207, ptr @PALLAS_SPEC_15, !35, !35, !208}
!207 = !{!"pallas.srcLoc", i64 58, i64 9, i64 58, i64 53, !38}
!208 = !{!209}
!209 = !{!199, !210}
!210 = !DILocalVariable(name: "s", arg: 1, scope: !211, file: !1, line: 58, type: !49)
!211 = distinct !DISubprogram(name: "PALLAS_SPEC_15", scope: !1, file: !1, line: 58, type: !46, scopeLine: 58, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !35)
!212 = !{!""}
!213 = !DILocation(line: 0, scope: !45)
!214 = !DILocation(line: 21, column: 16, scope: !45)
!215 = !DILocation(line: 0, scope: !81)
!216 = !DILocation(line: 26, column: 16, scope: !81)
!217 = !DILocation(line: 0, scope: !101)
!218 = !DILocation(line: 31, column: 16, scope: !101)
!219 = !DILocation(line: 0, scope: !121)
!220 = !DILocation(line: 36, column: 16, scope: !121)
!221 = !DILocation(line: 0, scope: !141)
!222 = !DILocation(line: 41, column: 16, scope: !141)
!223 = !DILocation(line: 0, scope: !161)
!224 = !DILocation(line: 46, column: 16, scope: !161)
!225 = !DILocation(line: 0, scope: !181)
!226 = !DILocation(line: 51, column: 16, scope: !181)
!227 = !DILocation(line: 0, scope: !201)
!228 = !DILocation(line: 56, column: 16, scope: !201)
!229 = !DILocation(line: 0, scope: !71)
!230 = !DILocation(line: 23, column: 16, scope: !71)
!231 = !DILocation(line: 23, column: 47, scope: !71)
!232 = !DILocation(line: 23, column: 32, scope: !71)
!233 = !DILocation(line: 23, column: 29, scope: !71)
!234 = !DILocation(line: 0, scope: !91)
!235 = !DILocation(line: 28, column: 31, scope: !91)
!236 = !DILocation(line: 28, column: 16, scope: !91)
!237 = !DILocation(line: 28, column: 50, scope: !91)
!238 = !DILocation(line: 28, column: 35, scope: !91)
!239 = !DILocation(line: 28, column: 33, scope: !91)
!240 = !DILocation(line: 0, scope: !111)
!241 = !DILocation(line: 33, column: 31, scope: !111)
!242 = !DILocation(line: 33, column: 16, scope: !111)
!243 = !DILocation(line: 33, column: 50, scope: !111)
!244 = !DILocation(line: 33, column: 35, scope: !111)
!245 = !DILocation(line: 33, column: 33, scope: !111)
!246 = !DILocation(line: 0, scope: !131)
!247 = !DILocation(line: 38, column: 31, scope: !131)
!248 = !DILocation(line: 38, column: 16, scope: !131)
!249 = !DILocation(line: 38, column: 50, scope: !131)
!250 = !DILocation(line: 38, column: 35, scope: !131)
!251 = !DILocation(line: 38, column: 33, scope: !131)
!252 = !DILocation(line: 0, scope: !151)
!253 = !DILocation(line: 43, column: 31, scope: !151)
!254 = !DILocation(line: 43, column: 16, scope: !151)
!255 = !DILocation(line: 43, column: 51, scope: !151)
!256 = !DILocation(line: 43, column: 53, scope: !151)
!257 = !DILocation(line: 43, column: 36, scope: !151)
!258 = !DILocation(line: 43, column: 33, scope: !151)
!259 = !DILocation(line: 0, scope: !171)
!260 = !DILocation(line: 48, column: 31, scope: !171)
!261 = !DILocation(line: 48, column: 33, scope: !171)
!262 = !DILocation(line: 48, column: 16, scope: !171)
!263 = !DILocation(line: 48, column: 52, scope: !171)
!264 = !DILocation(line: 48, column: 37, scope: !171)
!265 = !DILocation(line: 48, column: 35, scope: !171)
!266 = !DILocation(line: 0, scope: !191)
!267 = !DILocation(line: 53, column: 31, scope: !191)
!268 = !DILocation(line: 53, column: 33, scope: !191)
!269 = !DILocation(line: 53, column: 16, scope: !191)
!270 = !DILocation(line: 53, column: 52, scope: !191)
!271 = !DILocation(line: 53, column: 37, scope: !191)
!272 = !DILocation(line: 53, column: 35, scope: !191)
!273 = !DILocation(line: 0, scope: !211)
!274 = !DILocation(line: 58, column: 31, scope: !211)
!275 = !DILocation(line: 58, column: 33, scope: !211)
!276 = !DILocation(line: 58, column: 16, scope: !211)
!277 = !DILocation(line: 58, column: 52, scope: !211)
!278 = !DILocation(line: 58, column: 37, scope: !211)
!279 = !DILocation(line: 58, column: 35, scope: !211)
