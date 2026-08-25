; ModuleID = 'tmp/tmp_ir_source0.ll'
source_filename = "examples/concepts/llvm/pallas/pallas_byval.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.BigStruct = type { i64, i64, i64, i64 }
%pallas.fracT = type { i64, i64, i64, i64 }

@llvm.used = appending global [7 x ptr] [ptr @PALLAS_SPEC_0, ptr @PALLAS_SPEC_1, ptr @PALLAS_SPEC_2, ptr @PALLAS_SPEC_3, ptr @PALLAS_SPEC_4, ptr @PALLAS_SPEC_5, ptr @PALLAS_SPEC_6], section "llvm.metadata"

; Function Attrs: noinline nounwind uwtable
define dso_local void @do_a_thing(ptr noundef byval(%struct.BigStruct) align 8 %0) #0 !dbg !12 !pallas.fcontract !28 {
  call void @llvm.dbg.declare(metadata ptr %0, metadata !35, metadata !DIExpression()), !dbg !66
  %2 = getelementptr inbounds %struct.BigStruct, ptr %0, i32 0, i32 0, !dbg !67
  store i64 0, ptr %2, align 8, !dbg !68
  %3 = getelementptr inbounds %struct.BigStruct, ptr %0, i32 0, i32 0, !dbg !69
  %4 = load i64, ptr %3, align 8, !dbg !69
  %5 = add nsw i64 %4, 1, !dbg !70
  %6 = getelementptr inbounds %struct.BigStruct, ptr %0, i32 0, i32 1, !dbg !71
  store i64 %5, ptr %6, align 8, !dbg !72
  ret void, !dbg !73
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @a_function() #0 !dbg !74 !pallas.fcontract !78 {
  %1 = alloca %struct.BigStruct, align 8
  %2 = alloca i32, align 4
  call void @llvm.dbg.declare(metadata ptr %1, metadata !82, metadata !DIExpression()), !dbg !83
  %3 = getelementptr inbounds %struct.BigStruct, ptr %1, i32 0, i32 0, !dbg !84
  store i64 1, ptr %3, align 8, !dbg !85
  %4 = getelementptr inbounds %struct.BigStruct, ptr %1, i32 0, i32 1, !dbg !86
  store i64 2, ptr %4, align 8, !dbg !87
  call void @do_a_thing(ptr noundef byval(%struct.BigStruct) align 8 %1), !dbg !88
  call void @llvm.dbg.declare(metadata ptr %2, metadata !89, metadata !DIExpression()), !dbg !90
  %5 = getelementptr inbounds %struct.BigStruct, ptr %1, i32 0, i32 0, !dbg !91, !pallas.stmntBlock !92
  %6 = load i64, ptr %5, align 8, !dbg !91
  %7 = getelementptr inbounds %struct.BigStruct, ptr %1, i32 0, i32 1, !dbg !112
  %8 = load i64, ptr %7, align 8, !dbg !112
  %9 = add nsw i64 %6, %8, !dbg !113
  %10 = trunc i64 %9 to i32, !dbg !114
  store i32 %10, ptr %2, align 4, !dbg !90
  %11 = load i32, ptr %2, align 4, !dbg !115
  ret i32 %11, !dbg !116
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_0(ptr noundef byval(%struct.BigStruct) align 8 %0) #0 !dbg !37 !pallas.exprWrapper !117 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.declare(metadata ptr %0, metadata !36, metadata !DIExpression()), !dbg !118
  %3 = getelementptr inbounds %struct.BigStruct, ptr %0, i32 0, i32 0, !dbg !119
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !120
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !121
  ret i1 %4, !dbg !118
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_1(ptr noundef byval(%struct.BigStruct) align 8 %0) #0 !dbg !53 !pallas.exprWrapper !117 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.declare(metadata ptr %0, metadata !52, metadata !DIExpression()), !dbg !122
  %3 = getelementptr inbounds %struct.BigStruct, ptr %0, i32 0, i32 1, !dbg !123
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !124
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !125
  ret i1 %4, !dbg !122
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_2(ptr noundef byval(%struct.BigStruct) align 8 %0) #0 !dbg !59 !pallas.exprWrapper !117 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.declare(metadata ptr %0, metadata !58, metadata !DIExpression()), !dbg !126
  %3 = getelementptr inbounds %struct.BigStruct, ptr %0, i32 0, i32 2, !dbg !127
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !128
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !129
  ret i1 %4, !dbg !126
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_3(ptr noundef byval(%struct.BigStruct) align 8 %0) #0 !dbg !65 !pallas.exprWrapper !117 {
  %2 = alloca %pallas.fracT, align 8
  call void @llvm.dbg.declare(metadata ptr %0, metadata !64, metadata !DIExpression()), !dbg !130
  %3 = getelementptr inbounds %struct.BigStruct, ptr %0, i32 0, i32 3, !dbg !131
  call void @pallas.fracOf(ptr sret(%pallas.fracT) %2, i32 noundef 1, i32 noundef 1), !dbg !132
  %4 = call i1 @pallas.perm(ptr noundef %3, ptr noundef byval(%pallas.fracT) %2), !dbg !133
  ret i1 %4, !dbg !130
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_4() #0 !dbg !134 !pallas.exprWrapper !117 {
  %1 = call i32 @"pallas.result i32"(), !dbg !137
  %2 = icmp eq i32 %1, 3, !dbg !138
  ret i1 %2, !dbg !139
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_5(ptr noundef byval(%struct.BigStruct) align 8 %0, i32 noundef %1) #0 !dbg !99 !pallas.exprWrapper !117 {
  call void @llvm.dbg.declare(metadata ptr %0, metadata !98, metadata !DIExpression()), !dbg !140
  call void @llvm.dbg.value(metadata i32 %1, metadata !103, metadata !DIExpression()), !dbg !140
  %3 = getelementptr inbounds %struct.BigStruct, ptr %0, i32 0, i32 0, !dbg !141
  %4 = load i64, ptr %3, align 8, !dbg !141
  %5 = icmp eq i64 %4, 1, !dbg !142
  ret i1 %5, !dbg !140
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @PALLAS_SPEC_6(ptr noundef byval(%struct.BigStruct) align 8 %0, i32 noundef %1) #0 !dbg !109 !pallas.exprWrapper !117 {
  call void @llvm.dbg.declare(metadata ptr %0, metadata !108, metadata !DIExpression()), !dbg !143
  call void @llvm.dbg.value(metadata i32 %1, metadata !111, metadata !DIExpression()), !dbg !143
  %3 = getelementptr inbounds %struct.BigStruct, ptr %0, i32 0, i32 1, !dbg !144
  %4 = load i64, ptr %3, align 8, !dbg !144
  %5 = icmp eq i64 %4, 2, !dbg !145
  ret i1 %5, !dbg !143
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

declare !pallas.specLib !146 i1 @pallas.perm(ptr noundef, ptr noundef byval(%pallas.fracT))

declare !pallas.specLib !147 void @pallas.fracOf(ptr sret(%pallas.fracT), i32 noundef, i32 noundef)

declare !pallas.specLib !148 i32 @"pallas.result i32"()

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0, !2}
!llvm.module.flags = !{!4, !5, !6, !7, !8, !9, !10}
!llvm.ident = !{!11, !11}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/pallas/pallas_byval.c", directory: ".", checksumkind: CSK_MD5, checksum: "84d96dca12ec4f945b1d5ab18b4acdb3")
!2 = distinct !DICompileUnit(language: DW_LANG_C11, file: !3, producer: "clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "tmp/source_wrappers.c", directory: ".", checksumkind: CSK_MD5, checksum: "fc36b8a06cd0418f73f9fb877d647fc9")
!4 = !{i32 7, !"Dwarf Version", i32 5}
!5 = !{i32 2, !"Debug Info Version", i32 3}
!6 = !{i32 1, !"wchar_size", i32 4}
!7 = !{i32 8, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{i32 7, !"uwtable", i32 2}
!10 = !{i32 7, !"frame-pointer", i32 2}
!11 = !{!"clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 73500bf55acff5fa97b56dcdeb013f288efd084f)"}
!12 = distinct !DISubprogram(name: "do_a_thing", scope: !1, file: !1, line: 26, type: !13, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !27)
!13 = !DISubroutineType(types: !14)
!14 = !{null, !15}
!15 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !1, line: 18, baseType: !16)
!16 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !1, line: 13, size: 256, elements: !17)
!17 = !{!18, !24, !25, !26}
!18 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !16, file: !1, line: 14, baseType: !19, size: 64)
!19 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !20, line: 27, baseType: !21)
!20 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/stdint-intn.h", directory: "", checksumkind: CSK_MD5, checksum: "649b383a60bfa3eb90e85840b2b0be20")
!21 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !22, line: 44, baseType: !23)
!22 = !DIFile(filename: "/usr/include/x86_64-linux-gnu/bits/types.h", directory: "", checksumkind: CSK_MD5, checksum: "e1865d9fe29fe1b5ced550b7ba458f9e")
!23 = !DIBasicType(name: "long", size: 64, encoding: DW_ATE_signed)
!24 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !16, file: !1, line: 15, baseType: !19, size: 64, offset: 64)
!25 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !16, file: !1, line: 16, baseType: !19, size: 64, offset: 128)
!26 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !16, file: !1, line: 17, baseType: !19, size: 64, offset: 192)
!27 = !{}
!28 = !{!29, i1 false, i1 false, !27, !27, !31, !48, !54, !60}
!29 = !{!"pallas.srcLoc", i64 20, i64 1, i64 25, i64 1, !30}
!30 = !DIFile(filename: "/home/rme/repos/vercors/examples/concepts/llvm/pallas/pallas_byval.c", directory: "", checksumkind: CSK_MD5, checksum: "84d96dca12ec4f945b1d5ab18b4acdb3")
!31 = !{!"pallas.requires", !32, ptr @PALLAS_SPEC_0, !27, !27, !33}
!32 = !{!"pallas.srcLoc", i64 21, i64 1, i64 21, i64 29, !30}
!33 = !{!34}
!34 = !{!35, !36}
!35 = !DILocalVariable(name: "s", arg: 1, scope: !12, file: !1, line: 26, type: !15)
!36 = !DILocalVariable(name: "s", arg: 1, scope: !37, file: !1, line: 21, type: !41)
!37 = distinct !DISubprogram(name: "PALLAS_SPEC_0", scope: !1, file: !1, line: 21, type: !38, scopeLine: 21, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !27)
!38 = !DISubroutineType(types: !39)
!39 = !{!40, !41}
!40 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!41 = !DIDerivedType(tag: DW_TAG_typedef, name: "BigStruct", file: !3, line: 21, baseType: !42)
!42 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !3, line: 16, size: 256, elements: !43)
!43 = !{!44, !45, !46, !47}
!44 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !42, file: !3, line: 17, baseType: !19, size: 64)
!45 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !42, file: !3, line: 18, baseType: !19, size: 64, offset: 64)
!46 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !42, file: !3, line: 19, baseType: !19, size: 64, offset: 128)
!47 = !DIDerivedType(tag: DW_TAG_member, name: "d", scope: !42, file: !3, line: 20, baseType: !19, size: 64, offset: 192)
!48 = !{!"pallas.requires", !49, ptr @PALLAS_SPEC_1, !27, !27, !50}
!49 = !{!"pallas.srcLoc", i64 22, i64 1, i64 22, i64 29, !30}
!50 = !{!51}
!51 = !{!35, !52}
!52 = !DILocalVariable(name: "s", arg: 1, scope: !53, file: !1, line: 22, type: !41)
!53 = distinct !DISubprogram(name: "PALLAS_SPEC_1", scope: !1, file: !1, line: 22, type: !38, scopeLine: 22, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !27)
!54 = !{!"pallas.requires", !55, ptr @PALLAS_SPEC_2, !27, !27, !56}
!55 = !{!"pallas.srcLoc", i64 23, i64 1, i64 23, i64 29, !30}
!56 = !{!57}
!57 = !{!35, !58}
!58 = !DILocalVariable(name: "s", arg: 1, scope: !59, file: !1, line: 23, type: !41)
!59 = distinct !DISubprogram(name: "PALLAS_SPEC_2", scope: !1, file: !1, line: 23, type: !38, scopeLine: 23, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !27)
!60 = !{!"pallas.requires", !61, ptr @PALLAS_SPEC_3, !27, !27, !62}
!61 = !{!"pallas.srcLoc", i64 24, i64 1, i64 24, i64 29, !30}
!62 = !{!63}
!63 = !{!35, !64}
!64 = !DILocalVariable(name: "s", arg: 1, scope: !65, file: !1, line: 24, type: !41)
!65 = distinct !DISubprogram(name: "PALLAS_SPEC_3", scope: !1, file: !1, line: 24, type: !38, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !27)
!66 = !DILocation(line: 26, column: 27, scope: !12)
!67 = !DILocation(line: 27, column: 7, scope: !12)
!68 = !DILocation(line: 27, column: 9, scope: !12)
!69 = !DILocation(line: 28, column: 13, scope: !12)
!70 = !DILocation(line: 28, column: 15, scope: !12)
!71 = !DILocation(line: 28, column: 7, scope: !12)
!72 = !DILocation(line: 28, column: 9, scope: !12)
!73 = !DILocation(line: 29, column: 1, scope: !12)
!74 = distinct !DISubprogram(name: "a_function", scope: !1, file: !1, line: 34, type: !75, scopeLine: 34, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !27)
!75 = !DISubroutineType(types: !76)
!76 = !{!77}
!77 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!78 = !{!79, i1 false, i1 false, !27, !27, !80}
!79 = !{!"pallas.srcLoc", i64 31, i64 1, i64 33, i64 1, !30}
!80 = !{!"pallas.ensures", !81, ptr @PALLAS_SPEC_4, !27, !27, !27}
!81 = !{!"pallas.srcLoc", i64 32, i64 1, i64 32, i64 26, !30}
!82 = !DILocalVariable(name: "s", scope: !74, file: !1, line: 35, type: !15)
!83 = !DILocation(line: 35, column: 15, scope: !74)
!84 = !DILocation(line: 36, column: 7, scope: !74)
!85 = !DILocation(line: 36, column: 9, scope: !74)
!86 = !DILocation(line: 37, column: 7, scope: !74)
!87 = !DILocation(line: 37, column: 9, scope: !74)
!88 = !DILocation(line: 38, column: 5, scope: !74)
!89 = !DILocalVariable(name: "sum", scope: !74, file: !1, line: 44, type: !77)
!90 = !DILocation(line: 44, column: 9, scope: !74)
!91 = !DILocation(line: 44, column: 17, scope: !74)
!92 = !{!93, !94, !104}
!93 = !{!"pallas.srcLoc", i64 40, i64 5, i64 43, i64 5, !30}
!94 = !{!"pallas.assert", !95, ptr @PALLAS_SPEC_5, !27, !27, !96}
!95 = !{!"pallas.srcLoc", i64 41, i64 5, i64 41, i64 20, !30}
!96 = !{!97, !102}
!97 = !{!82, !98}
!98 = !DILocalVariable(name: "s", arg: 1, scope: !99, file: !1, line: 41, type: !41)
!99 = distinct !DISubprogram(name: "PALLAS_SPEC_5", scope: !1, file: !1, line: 41, type: !100, scopeLine: 41, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !27)
!100 = !DISubroutineType(types: !101)
!101 = !{!40, !41, !77}
!102 = !{!89, !103}
!103 = !DILocalVariable(name: "sum", arg: 2, scope: !99, file: !1, line: 41, type: !77)
!104 = !{!"pallas.assert", !105, ptr @PALLAS_SPEC_6, !27, !27, !106}
!105 = !{!"pallas.srcLoc", i64 42, i64 5, i64 42, i64 20, !30}
!106 = !{!107, !110}
!107 = !{!82, !108}
!108 = !DILocalVariable(name: "s", arg: 1, scope: !109, file: !1, line: 42, type: !41)
!109 = distinct !DISubprogram(name: "PALLAS_SPEC_6", scope: !1, file: !1, line: 42, type: !100, scopeLine: 42, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !27)
!110 = !{!89, !111}
!111 = !DILocalVariable(name: "sum", arg: 2, scope: !109, file: !1, line: 42, type: !77)
!112 = !DILocation(line: 44, column: 23, scope: !74)
!113 = !DILocation(line: 44, column: 19, scope: !74)
!114 = !DILocation(line: 44, column: 15, scope: !74)
!115 = !DILocation(line: 45, column: 12, scope: !74)
!116 = !DILocation(line: 45, column: 5, scope: !74)
!117 = !{!""}
!118 = !DILocation(line: 0, scope: !37)
!119 = !DILocation(line: 21, column: 19, scope: !37)
!120 = !DILocation(line: 21, column: 22, scope: !37)
!121 = !DILocation(line: 21, column: 10, scope: !37)
!122 = !DILocation(line: 0, scope: !53)
!123 = !DILocation(line: 22, column: 19, scope: !53)
!124 = !DILocation(line: 22, column: 22, scope: !53)
!125 = !DILocation(line: 22, column: 10, scope: !53)
!126 = !DILocation(line: 0, scope: !59)
!127 = !DILocation(line: 23, column: 19, scope: !59)
!128 = !DILocation(line: 23, column: 22, scope: !59)
!129 = !DILocation(line: 23, column: 10, scope: !59)
!130 = !DILocation(line: 0, scope: !65)
!131 = !DILocation(line: 24, column: 19, scope: !65)
!132 = !DILocation(line: 24, column: 22, scope: !65)
!133 = !DILocation(line: 24, column: 10, scope: !65)
!134 = distinct !DISubprogram(name: "PALLAS_SPEC_4", scope: !1, file: !1, line: 32, type: !135, scopeLine: 32, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0)
!135 = !DISubroutineType(types: !136)
!136 = !{!40}
!137 = !DILocation(line: 32, column: 9, scope: !134)
!138 = !DILocation(line: 32, column: 22, scope: !134)
!139 = !DILocation(line: 0, scope: !134)
!140 = !DILocation(line: 0, scope: !99)
!141 = !DILocation(line: 41, column: 14, scope: !99)
!142 = !DILocation(line: 41, column: 16, scope: !99)
!143 = !DILocation(line: 0, scope: !109)
!144 = !DILocation(line: 42, column: 14, scope: !109)
!145 = !DILocation(line: 42, column: 16, scope: !109)
!146 = !{!"pallas.perm"}
!147 = !{!"pallas.fracOf"}
!148 = !{!"pallas.result"}
