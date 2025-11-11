; ModuleID = 'tmp/tmp_ir_source.ll'
source_filename = "tmp/tmp_ir_source.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%TSi = type <{ i64 }>

@"\01l_entry_point" = private constant { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @main to i64), i64 ptrtoint (ptr @"\01l_entry_point" to i64)) to i32), i32 0 }, section "swift5_entry", align 4
@_swift1_autolink_entries = private constant [102 x i8] c"-lswiftSwiftOnoneSupport\00-lswiftCore\00-lswift_Concurrency\00-lswift_StringProcessing\00-lswift_RegexParser\00", section ".swift1_autolink_entries", no_sanitize_address, align 8
@llvm.used = appending global [8 x ptr] [ptr @main, ptr @"$s13tmp_ir_source3fooyS2i_SitF", ptr @"\01l_entry_point", ptr @__swift_reflection_version, ptr @_swift1_autolink_entries, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01a1b0A0SbSi_S2itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_11a1b0A0SbSi_S2itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_21a1b0A0SbSi_S2itF"], section "llvm.metadata"
@__swift_reflection_version = linkonce_odr hidden constant i16 3

define protected i32 @main(i32 %0, ptr %1) #0 !dbg !45 {
entry:
  ret i32 0, !dbg !50
}

define hidden swiftcc i64 @"$s13tmp_ir_source3fooyS2i_SitF"(i64 %0, i64 %1) #0 !dbg !53 {
entry:
  %2 = alloca %TSi, align 8
  call void @llvm.dbg.declare(metadata ptr %2, metadata !63, metadata !DIExpression()), !dbg !65
  call void @llvm.memset.p0.i64(ptr align 8 %2, i8 0, i64 8, i1 false)
  call void @llvm.dbg.value(metadata i64 %0, metadata !60, metadata !DIExpression()), !dbg !66
  call void @llvm.dbg.value(metadata i64 %1, metadata !62, metadata !DIExpression()), !dbg !67
  call void @llvm.lifetime.start.p0(i64 8, ptr %2), !dbg !68
  %._value = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !70
  store i64 %0, ptr %._value, align 8, !dbg !70
  %3 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %0, i64 %1), !dbg !72, !pallas.stmntBlock !73
  %4 = extractvalue { i64, i1 } %3, 0, !dbg !72
  %5 = extractvalue { i64, i1 } %3, 1, !dbg !72
  %6 = call i1 @llvm.expect.i1(i1 %5, i1 false), !dbg !72
  br i1 %6, label %18, label %7, !dbg !72

7:                                                ; preds = %entry
  %._value1 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !72
  store i64 %4, ptr %._value1, align 8, !dbg !72
  %8 = icmp slt i64 %4, 0, !dbg !78, !pallas.stmntBlock !80
  br i1 %8, label %9, label %15, !dbg !78

9:                                                ; preds = %7
  %10 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %4, i64 -1), !dbg !84
  %11 = extractvalue { i64, i1 } %10, 0, !dbg !84
  %12 = extractvalue { i64, i1 } %10, 1, !dbg !84
  %13 = call i1 @llvm.expect.i1(i1 %12, i1 false), !dbg !84
  br i1 %13, label %19, label %14, !dbg !84

14:                                               ; preds = %9
  %._value3 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !84
  store i64 %11, ptr %._value3, align 8, !dbg !84
  br label %16, !dbg !86

15:                                               ; preds = %7
  br label %16, !dbg !87

16:                                               ; preds = %15, %14
  %._value2 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !88
  %17 = load i64, ptr %._value2, align 8, !dbg !88
  call void @llvm.lifetime.end.p0(i64 8, ptr %2), !dbg !89, !pallas.stmntBlock !90
  ret i64 %17, !dbg !89

18:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !94
  unreachable, !dbg !94

19:                                               ; preds = %9
  call void @llvm.trap(), !dbg !97
  unreachable, !dbg !97
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #2

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #3

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare i1 @llvm.expect.i1(i1, i1) #4

; Function Attrs: cold noreturn nounwind
declare void @llvm.trap() #5

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #2

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64) #1

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_01a1b0A0SbSi_S2itF"(i64 %0, i64 %1, i64 %2) #0 !dbg !98 !pallas.exprWrapper !102 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !103, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i64 %1, metadata !105, metadata !DIExpression()), !dbg !104
  call void @llvm.dbg.value(metadata i64 %2, metadata !106, metadata !DIExpression()), !dbg !104
  %3 = icmp slt i64 0, %2, !dbg !107
  ret i1 %3, !dbg !104
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_11a1b0A0SbSi_S2itF"(i64 %0, i64 %1, i64 %2) #0 !dbg !108 !pallas.exprWrapper !102 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !109, metadata !DIExpression()), !dbg !110
  call void @llvm.dbg.value(metadata i64 %1, metadata !111, metadata !DIExpression()), !dbg !110
  call void @llvm.dbg.value(metadata i64 %2, metadata !112, metadata !DIExpression()), !dbg !110
  %3 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %0, i64 %1), !dbg !113
  %4 = extractvalue { i64, i1 } %3, 0, !dbg !113
  %5 = extractvalue { i64, i1 } %3, 1, !dbg !113
  %6 = call i1 @llvm.expect.i1(i1 %5, i1 false), !dbg !113
  br i1 %6, label %9, label %7, !dbg !113

7:                                                ; preds = %entry
  %8 = icmp eq i64 %2, %4, !dbg !114
  ret i1 %8, !dbg !110

9:                                                ; preds = %entry
  call void @llvm.trap(), !dbg !115
  unreachable, !dbg !115
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_21a1b0A0SbSi_S2itF"(i64 %0, i64 %1, i64 %2) #0 !dbg !117 !pallas.exprWrapper !102 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !118, metadata !DIExpression()), !dbg !119
  call void @llvm.dbg.value(metadata i64 %1, metadata !120, metadata !DIExpression()), !dbg !119
  call void @llvm.dbg.value(metadata i64 %2, metadata !121, metadata !DIExpression()), !dbg !119
  %3 = icmp slt i64 %2, 0, !dbg !122
  %4 = xor i1 %3, true, !dbg !122
  ret i1 %4, !dbg !119
}

attributes #0 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #3 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #4 = { nocallback nofree nosync nounwind willreturn memory(none) }
attributes #5 = { cold noreturn nounwind }

!llvm.dbg.cu = !{!0, !15, !17, !19, !31, !32}
!swift.module.flags = !{!33, !33}
!llvm.linker.options = !{}
!llvm.module.flags = !{!34, !35, !36, !37, !38, !39, !40, !41, !42, !43, !44}

!0 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !1, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !2)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_swift_assert.swift", directory: "/home/rme/repos/vercors")
!2 = !{!3, !5, !7, !9, !11, !13}
!3 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !4, file: !1)
!4 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "examples/concepts/llvm/pallas")
!5 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !6, file: !1)
!6 = !DIModule(scope: null, name: "Swift", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!7 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !8, file: !1)
!8 = !DIModule(scope: null, name: "_StringProcessing", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/_StringProcessing.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!9 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !10, file: !1)
!10 = !DIModule(scope: null, name: "_SwiftConcurrencyShims", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/shims")
!11 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !12, file: !1)
!12 = !DIModule(scope: null, name: "_Concurrency", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/_Concurrency.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!13 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !14, file: !1)
!14 = !DIModule(scope: null, name: "SwiftOnoneSupport", includePath: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/SwiftOnoneSupport.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!15 = distinct !DICompileUnit(language: DW_LANG_C11, file: !16, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!16 = !DIFile(filename: "<swift-imported-modules>", directory: "/home/rme/repos/vercors")
!17 = distinct !DICompileUnit(language: DW_LANG_C99, file: !18, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "/home/rme/.cache/clang/ModuleCache/1T7NA3LBRX57T/_SwiftConcurrencyShims-16QL5XP1HZ73F.pcm", emissionKind: FullDebug, dwoId: 205401482013525099)
!18 = !DIFile(filename: "_SwiftConcurrencyShims", directory: "/home/rme/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/shims")
!19 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !20, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !21)
!20 = !DIFile(filename: "tmp/source_wrappers.swift", directory: "/home/rme/repos/vercors")
!21 = !{!22, !24, !25, !27, !28, !29, !30}
!22 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !23, file: !20)
!23 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "tmp")
!24 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !6, file: !20)
!25 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !26, file: !20, line: 1)
!26 = !DIModule(scope: null, name: "PallasSpec", includePath: "/home/rme/repos/pallas_spec2ir/res/spec_libs/swift/PallasSpec/.build/debug/Modules/PallasSpec.swiftmodule")
!27 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !8, file: !20)
!28 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !10, file: !20)
!29 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !12, file: !20)
!30 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !14, file: !20)
!31 = distinct !DICompileUnit(language: DW_LANG_C11, file: !16, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!32 = distinct !DICompileUnit(language: DW_LANG_C99, file: !18, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "/home/rme/.cache/clang/ModuleCache/1T7NA3LBRX57T/_SwiftConcurrencyShims-16QL5XP1HZ73F.pcm", emissionKind: FullDebug, dwoId: 205401482013525099)
!33 = !{!"standard-library", i1 false}
!34 = !{i32 7, !"Dwarf Version", i32 4}
!35 = !{i32 2, !"Debug Info Version", i32 3}
!36 = !{i32 1, !"wchar_size", i32 4}
!37 = !{i32 8, !"PIC Level", i32 2}
!38 = !{i32 7, !"uwtable", i32 2}
!39 = !{i32 7, !"frame-pointer", i32 2}
!40 = !{i32 1, !"Objective-C Garbage Collection", i8 0}
!41 = !{i32 1, !"Swift Version", i32 7}
!42 = !{i32 1, !"Swift ABI Version", i32 7}
!43 = !{i32 1, !"Swift Major Version", i8 6}
!44 = !{i32 1, !"Swift Minor Version", i8 0}
!45 = distinct !DISubprogram(name: "main", linkageName: "main", scope: !4, file: !1, line: 1, type: !46, spFlags: DISPFlagDefinition, unit: !0)
!46 = !DISubroutineType(types: !47)
!47 = !{!48, !48, !49}
!48 = !DICompositeType(tag: DW_TAG_structure_type, name: "$ss5Int32VD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!49 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSpySpys4Int8VGSgGD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!50 = !DILocation(line: 0, scope: !51)
!51 = !DILexicalBlockFile(scope: !45, file: !52, discriminator: 0)
!52 = !DIFile(filename: "<compiler-generated>", directory: "/")
!53 = distinct !DISubprogram(name: "foo", linkageName: "$s13tmp_ir_source3fooyS2i_SitF", scope: !4, file: !1, line: 7, type: !54, scopeLine: 7, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !59)
!54 = !DISubroutineType(types: !55)
!55 = !{!56, !56, !56}
!56 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !6, file: !57, size: 64, elements: !58, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!57 = !DIFile(filename: "swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule", directory: "/home/rme")
!58 = !{}
!59 = !{!60, !62, !63}
!60 = !DILocalVariable(name: "a", arg: 1, scope: !53, file: !1, line: 7, type: !61)
!61 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !56)
!62 = !DILocalVariable(name: "b", arg: 2, scope: !53, file: !1, line: 7, type: !61)
!63 = !DILocalVariable(name: "tmp", scope: !64, file: !1, line: 8, type: !56)
!64 = distinct !DILexicalBlock(scope: !53, file: !1, line: 8, column: 9)
!65 = !DILocation(line: 8, column: 9, scope: !64)
!66 = !DILocation(line: 7, column: 10, scope: !53)
!67 = !DILocation(line: 7, column: 20, scope: !53)
!68 = !DILocation(line: 0, scope: !69)
!69 = !DILexicalBlockFile(scope: !64, discriminator: 0)
!70 = !DILocation(line: 8, column: 15, scope: !71)
!71 = distinct !DILexicalBlock(scope: !53, file: !1, line: 8, column: 15)
!72 = !DILocation(line: 10, column: 9, scope: !64)
!73 = !{!74, !76}
!74 = !{!"pallas.srcLoc", i64 9, i64 5, i64 9, i64 25, !75}
!75 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_swift_assert.swift", directory: "", checksumkind: CSK_MD5, checksum: "55d5f5db49074fae743cbc42073d5564")
!76 = !{!"pallas.assume", !77, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01a1b0A0SbSi_S2itF", !60, !62, !63}
!77 = !{!"pallas.srcLoc", i64 9, i64 9, i64 9, i64 23, !75}
!78 = !DILocation(line: 13, column: 12, scope: !79)
!79 = distinct !DILexicalBlock(scope: !64, file: !1, line: 13, column: 5)
!80 = !{!81, !82}
!81 = !{!"pallas.srcLoc", i64 11, i64 5, i64 11, i64 30, !75}
!82 = !{!"pallas.assert", !83, ptr @"$s13tmp_ir_source13PALLAS_SPEC_11a1b0A0SbSi_S2itF", !60, !62, !63}
!83 = !{!"pallas.srcLoc", i64 11, i64 9, i64 11, i64 28, !75}
!84 = !DILocation(line: 14, column: 13, scope: !85)
!85 = distinct !DILexicalBlock(scope: !79, file: !1, line: 13, column: 16)
!86 = !DILocation(line: 15, column: 5, scope: !85)
!87 = !DILocation(line: 13, column: 5, scope: !79)
!88 = !DILocation(line: 0, scope: !85)
!89 = !DILocation(line: 18, column: 5, scope: !64)
!90 = !{!91, !92}
!91 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 26, !75}
!92 = !{!"pallas.assert", !93, ptr @"$s13tmp_ir_source13PALLAS_SPEC_21a1b0A0SbSi_S2itF", !60, !62, !63}
!93 = !{!"pallas.srcLoc", i64 17, i64 9, i64 17, i64 24, !75}
!94 = !DILocation(line: 0, scope: !95, inlinedAt: !72)
!95 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !96, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !0)
!96 = !DISubroutineType(types: null)
!97 = !DILocation(line: 0, scope: !95, inlinedAt: !84)
!98 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_01a1b0A0SbSi_S2itF", scope: !4, file: !1, line: 9, type: !99, scopeLine: 9, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!99 = !DISubroutineType(types: !100)
!100 = !{!101, !56, !56, !56}
!101 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSbD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!102 = !{!""}
!103 = !DILocalVariable(name: "a", arg: 1, scope: !98, file: !1, line: 9, type: !61)
!104 = !DILocation(line: 0, scope: !98)
!105 = !DILocalVariable(name: "b", arg: 2, scope: !98, file: !1, line: 9, type: !61)
!106 = !DILocalVariable(name: "tmp", arg: 3, scope: !98, file: !1, line: 9, type: !61)
!107 = !DILocation(line: 9, column: 20, scope: !98)
!108 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_11a1b0A0SbSi_S2itF", scope: !4, file: !1, line: 11, type: !99, scopeLine: 11, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!109 = !DILocalVariable(name: "a", arg: 1, scope: !108, file: !1, line: 11, type: !61)
!110 = !DILocation(line: 0, scope: !108)
!111 = !DILocalVariable(name: "b", arg: 2, scope: !108, file: !1, line: 11, type: !61)
!112 = !DILocalVariable(name: "tmp", arg: 3, scope: !108, file: !1, line: 11, type: !61)
!113 = !DILocation(line: 11, column: 25, scope: !108)
!114 = !DILocation(line: 11, column: 20, scope: !108)
!115 = !DILocation(line: 0, scope: !116, inlinedAt: !113)
!116 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !96, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
!117 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_21a1b0A0SbSi_S2itF", scope: !4, file: !1, line: 17, type: !99, scopeLine: 17, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!118 = !DILocalVariable(name: "a", arg: 1, scope: !117, file: !1, line: 17, type: !61)
!119 = !DILocation(line: 0, scope: !117)
!120 = !DILocalVariable(name: "b", arg: 2, scope: !117, file: !1, line: 17, type: !61)
!121 = !DILocalVariable(name: "tmp", arg: 3, scope: !117, file: !1, line: 17, type: !61)
!122 = !DILocation(line: 17, column: 20, scope: !117)
