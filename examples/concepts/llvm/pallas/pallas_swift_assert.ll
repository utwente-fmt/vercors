; ModuleID = './tmp/tmp_ir_source.ll'
source_filename = "tmp/tmp_ir_source.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%TSi = type <{ i64 }>

@"\01l_entry_point" = private constant { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @main to i64), i64 ptrtoint (ptr @"\01l_entry_point" to i64)) to i32), i32 0 }, section "swift5_entry", align 4
@_swift1_autolink_entries = private constant [102 x i8] c"-lswiftSwiftOnoneSupport\00-lswiftCore\00-lswift_Concurrency\00-lswift_StringProcessing\00-lswift_RegexParser\00", section ".swift1_autolink_entries", no_sanitize_address, align 8
@llvm.used = appending global [8 x ptr] [ptr @main, ptr @"$s13tmp_ir_source3fooyS2i_SitF", ptr @"\01l_entry_point", ptr @__swift_reflection_version, ptr @_swift1_autolink_entries, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01a1b0A0SbSi_S2itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_21a1b0A0SbSi_S2itF", ptr @"$s13tmp_ir_source13PALLAS_SPEC_11a1b0A0SbSi_S2itF"], section "llvm.metadata"
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
  %8 = icmp slt i64 %4, 0, !dbg !77, !pallas.stmntBlock !79
  br i1 %8, label %9, label %15, !dbg !77

9:                                                ; preds = %7
  %10 = call { i64, i1 } @llvm.smul.with.overflow.i64(i64 %4, i64 -1), !dbg !83
  %11 = extractvalue { i64, i1 } %10, 0, !dbg !83
  %12 = extractvalue { i64, i1 } %10, 1, !dbg !83
  %13 = call i1 @llvm.expect.i1(i1 %12, i1 false), !dbg !83
  br i1 %13, label %19, label %14, !dbg !83

14:                                               ; preds = %9
  %._value3 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !83
  store i64 %11, ptr %._value3, align 8, !dbg !83
  br label %16, !dbg !85

15:                                               ; preds = %7
  br label %16, !dbg !86

16:                                               ; preds = %15, %14
  %._value2 = getelementptr inbounds %TSi, ptr %2, i32 0, i32 0, !dbg !87
  %17 = load i64, ptr %._value2, align 8, !dbg !87
  call void @llvm.lifetime.end.p0(i64 8, ptr %2), !dbg !88, !pallas.stmntBlock !89
  ret i64 %17, !dbg !88

18:                                               ; preds = %entry
  call void @llvm.trap(), !dbg !93
  unreachable, !dbg !93

19:                                               ; preds = %9
  call void @llvm.trap(), !dbg !96
  unreachable, !dbg !96
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

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_01a1b0A0SbSi_S2itF"(i64 %0, i64 %1, i64 %2) #0 !dbg !97 !pallas.exprWrapper !101 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !102, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i64 %1, metadata !104, metadata !DIExpression()), !dbg !103
  call void @llvm.dbg.value(metadata i64 %2, metadata !105, metadata !DIExpression()), !dbg !103
  %3 = icmp slt i64 0, %2, !dbg !106
  ret i1 %3, !dbg !103
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_21a1b0A0SbSi_S2itF"(i64 %0, i64 %1, i64 %2) #0 !dbg !107 !pallas.exprWrapper !101 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !108, metadata !DIExpression()), !dbg !109
  call void @llvm.dbg.value(metadata i64 %1, metadata !110, metadata !DIExpression()), !dbg !109
  call void @llvm.dbg.value(metadata i64 %2, metadata !111, metadata !DIExpression()), !dbg !109
  %3 = icmp slt i64 %2, 0, !dbg !112
  %4 = xor i1 %3, true, !dbg !112
  ret i1 %4, !dbg !109
}

define hidden swiftcc i1 @"$s13tmp_ir_source13PALLAS_SPEC_11a1b0A0SbSi_S2itF"(i64 %0, i64 %1, i64 %2) #0 !dbg !113 !pallas.exprWrapper !101 {
entry:
  call void @llvm.dbg.value(metadata i64 %0, metadata !114, metadata !DIExpression()), !dbg !115
  call void @llvm.dbg.value(metadata i64 %1, metadata !116, metadata !DIExpression()), !dbg !115
  call void @llvm.dbg.value(metadata i64 %2, metadata !117, metadata !DIExpression()), !dbg !115
  %3 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %0, i64 %1), !dbg !118
  %4 = extractvalue { i64, i1 } %3, 0, !dbg !118
  %5 = extractvalue { i64, i1 } %3, 1, !dbg !118
  %6 = call i1 @llvm.expect.i1(i1 %5, i1 false), !dbg !118
  br i1 %6, label %9, label %7, !dbg !118

7:                                                ; preds = %entry
  %8 = icmp eq i64 %2, %4, !dbg !119
  ret i1 %8, !dbg !115

9:                                                ; preds = %entry
  call void @llvm.trap(), !dbg !120
  unreachable, !dbg !120
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
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_swift_assert.swift", directory: "/home/vercors")
!2 = !{!3, !5, !7, !9, !11, !13}
!3 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !4, file: !1)
!4 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "examples/concepts/llvm/pallas")
!5 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !6, file: !1)
!6 = !DIModule(scope: null, name: "Swift", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!7 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !8, file: !1)
!8 = !DIModule(scope: null, name: "_StringProcessing", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/_StringProcessing.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!9 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !10, file: !1)
!10 = !DIModule(scope: null, name: "_SwiftConcurrencyShims", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/shims")
!11 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !12, file: !1)
!12 = !DIModule(scope: null, name: "_Concurrency", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/_Concurrency.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!13 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1, entity: !14, file: !1)
!14 = !DIModule(scope: null, name: "SwiftOnoneSupport", includePath: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/SwiftOnoneSupport.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule")
!15 = distinct !DICompileUnit(language: DW_LANG_C11, file: !16, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!16 = !DIFile(filename: "<swift-imported-modules>", directory: "/home/USERNAME/repos/vercors")
!17 = distinct !DICompileUnit(language: DW_LANG_C99, file: !18, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "/home/USERNAME/.cache/clang/ModuleCache/1T7NA3LBRX57T/_SwiftConcurrencyShims-16QL5XP1HZ73F.pcm", emissionKind: FullDebug, dwoId: 205401482013525099)
!18 = !DIFile(filename: "_SwiftConcurrencyShims", directory: "/home/USERNAME/swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/shims")
!19 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !20, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, imports: !21)
!20 = !DIFile(filename: "tmp/source_wrappers.swift", directory: "/home/USERNAME/repos/vercors")
!21 = !{!22, !24, !25, !27, !28, !29, !30}
!22 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !23, file: !20)
!23 = !DIModule(scope: null, name: "tmp_ir_source", includePath: "tmp")
!24 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !6, file: !20)
!25 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !26, file: !20, line: 1)
!26 = !DIModule(scope: null, name: "PallasSpec", includePath: "/home/USERNAME/repos/vercors/../pallas_spec2ir/res/spec_libs/swift/PallasSpec/.build/debug/Modules/PallasSpec.swiftmodule")
!27 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !8, file: !20)
!28 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !10, file: !20)
!29 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !12, file: !20)
!30 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !20, entity: !14, file: !20)
!31 = distinct !DICompileUnit(language: DW_LANG_C11, file: !16, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!32 = distinct !DICompileUnit(language: DW_LANG_C99, file: !18, producer: "Swift version 6.0 (swift-6.0-RELEASE)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "/home/USERNAME/.cache/clang/ModuleCache/1T7NA3LBRX57T/_SwiftConcurrencyShims-16QL5XP1HZ73F.pcm", emissionKind: FullDebug, dwoId: 205401482013525099)
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
!57 = !DIFile(filename: "swift/swift-6.0-RELEASE-ubuntu24.04/usr/lib/swift/linux/Swift.swiftmodule/x86_64-unknown-linux-gnu.swiftmodule", directory: "/home/USERNAME")
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
!73 = !{!74, !75}
!74 = !{!"pallas.srcLoc", i64 9, i64 5, i64 9, i64 25}
!75 = !{!"pallas.assume", !76, ptr @"$s13tmp_ir_source13PALLAS_SPEC_01a1b0A0SbSi_S2itF", !60, !62, !63}
!76 = !{!"pallas.srcLoc", i64 9, i64 9, i64 9, i64 23}
!77 = !DILocation(line: 13, column: 12, scope: !78)
!78 = distinct !DILexicalBlock(scope: !64, file: !1, line: 13, column: 5)
!79 = !{!80, !81}
!80 = !{!"pallas.srcLoc", i64 11, i64 5, i64 11, i64 30}
!81 = !{!"pallas.assert", !82, ptr @"$s13tmp_ir_source13PALLAS_SPEC_11a1b0A0SbSi_S2itF", !60, !62, !63}
!82 = !{!"pallas.srcLoc", i64 11, i64 9, i64 11, i64 28}
!83 = !DILocation(line: 14, column: 13, scope: !84)
!84 = distinct !DILexicalBlock(scope: !78, file: !1, line: 13, column: 16)
!85 = !DILocation(line: 15, column: 5, scope: !84)
!86 = !DILocation(line: 13, column: 5, scope: !78)
!87 = !DILocation(line: 0, scope: !84)
!88 = !DILocation(line: 18, column: 5, scope: !64)
!89 = !{!90, !91}
!90 = !{!"pallas.srcLoc", i64 17, i64 5, i64 17, i64 26}
!91 = !{!"pallas.assert", !92, ptr @"$s13tmp_ir_source13PALLAS_SPEC_21a1b0A0SbSi_S2itF", !60, !62, !63}
!92 = !{!"pallas.srcLoc", i64 17, i64 9, i64 17, i64 24}
!93 = !DILocation(line: 0, scope: !94, inlinedAt: !72)
!94 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !95, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !0)
!95 = !DISubroutineType(types: null)
!96 = !DILocation(line: 0, scope: !94, inlinedAt: !83)
!97 = distinct !DISubprogram(name: "PALLAS_SPEC_0", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_01a1b0A0SbSi_S2itF", scope: !4, file: !1, line: 9, type: !98, scopeLine: 9, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!98 = !DISubroutineType(types: !99)
!99 = !{!100, !56, !56, !56}
!100 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSbD", scope: !6, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift)
!101 = !{!""}
!102 = !DILocalVariable(name: "a", arg: 1, scope: !97, file: !1, line: 9, type: !61)
!103 = !DILocation(line: 0, scope: !97)
!104 = !DILocalVariable(name: "b", arg: 2, scope: !97, file: !1, line: 9, type: !61)
!105 = !DILocalVariable(name: "tmp", arg: 3, scope: !97, file: !1, line: 9, type: !61)
!106 = !DILocation(line: 9, column: 20, scope: !97)
!107 = distinct !DISubprogram(name: "PALLAS_SPEC_2", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_21a1b0A0SbSi_S2itF", scope: !4, file: !1, line: 17, type: !98, scopeLine: 17, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!108 = !DILocalVariable(name: "a", arg: 1, scope: !107, file: !1, line: 17, type: !61)
!109 = !DILocation(line: 0, scope: !107)
!110 = !DILocalVariable(name: "b", arg: 2, scope: !107, file: !1, line: 17, type: !61)
!111 = !DILocalVariable(name: "tmp", arg: 3, scope: !107, file: !1, line: 17, type: !61)
!112 = !DILocation(line: 17, column: 20, scope: !107)
!113 = distinct !DISubprogram(name: "PALLAS_SPEC_1", linkageName: "$s13tmp_ir_source13PALLAS_SPEC_11a1b0A0SbSi_S2itF", scope: !4, file: !1, line: 11, type: !98, scopeLine: 11, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !58)
!114 = !DILocalVariable(name: "a", arg: 1, scope: !113, file: !1, line: 11, type: !61)
!115 = !DILocation(line: 0, scope: !113)
!116 = !DILocalVariable(name: "b", arg: 2, scope: !113, file: !1, line: 11, type: !61)
!117 = !DILocalVariable(name: "tmp", arg: 3, scope: !113, file: !1, line: 11, type: !61)
!118 = !DILocation(line: 11, column: 25, scope: !113)
!119 = !DILocation(line: 11, column: 20, scope: !113)
!120 = !DILocation(line: 0, scope: !121, inlinedAt: !118)
!121 = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: !52, file: !52, type: !95, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !19)
