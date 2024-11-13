; ModuleID = 'examples/concepts/llvm/cubed.c'
source_filename = "examples/concepts/llvm/cubed.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.TestStruct = type { i32, i32 }

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @cubed(i32 noundef %0) #0 !dbg !10 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !15, metadata !DIExpression()), !dbg !16
  call void @llvm.dbg.declare(metadata ptr %3, metadata !17, metadata !DIExpression()), !dbg !18
  store i32 0, ptr %3, align 4, !dbg !18
  call void @llvm.dbg.declare(metadata ptr %4, metadata !19, metadata !DIExpression()), !dbg !20
  store i32 0, ptr %4, align 4, !dbg !20
  call void @cubed_loop1_invariant_assert(ptr noundef %3, ptr noundef %4, ptr noundef %2), !dbg !21
  br label %5, !dbg !22

5:                                                ; preds = %1, %10
  call void @cubed_loop1_invariant_assume(ptr noundef %3, ptr noundef %4, ptr noundef %2), !dbg !23
  %6 = load i32, ptr %3, align 4, !dbg !25
  %7 = load i32, ptr %2, align 4, !dbg !27
  %8 = icmp sge i32 %6, %7, !dbg !28
  br i1 %8, label %9, label %10, !dbg !29

9:                                                ; preds = %5
  br label %18, !dbg !30

10:                                               ; preds = %5
  %11 = load i32, ptr %4, align 4, !dbg !32
  %12 = load i32, ptr %2, align 4, !dbg !33
  %13 = load i32, ptr %2, align 4, !dbg !34
  %14 = mul nsw i32 %12, %13, !dbg !35
  %15 = add nsw i32 %11, %14, !dbg !36
  store i32 %15, ptr %4, align 4, !dbg !37
  %16 = load i32, ptr %3, align 4, !dbg !38
  %17 = add nsw i32 %16, 1, !dbg !39
  store i32 %17, ptr %3, align 4, !dbg !40
  call void @cubed_loop1_invariant_assert(ptr noundef %3, ptr noundef %4, ptr noundef %2), !dbg !41
  br label %5, !dbg !22, !llvm.loop !42

18:                                               ; preds = %9
  %19 = load i32, ptr %4, align 4, !dbg !44
  ret i32 %19, !dbg !45
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare void @cubed_loop1_invariant_assert(ptr noundef, ptr noundef, ptr noundef) #2

declare void @cubed_loop1_invariant_assume(ptr noundef, ptr noundef, ptr noundef) #2

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @complicatedFunction() #0 !dbg !46 {
  %1 = alloca %struct.TestStruct, align 4
  call void @llvm.dbg.declare(metadata ptr %1, metadata !49, metadata !DIExpression()), !dbg !54
  %2 = getelementptr inbounds %struct.TestStruct, ptr %1, i32 0, i32 0, !dbg !55
  store i32 10, ptr %2, align 4, !dbg !56
  %3 = getelementptr inbounds %struct.TestStruct, ptr %1, i32 0, i32 0, !dbg !57
  %4 = load i32, ptr %3, align 4, !dbg !57
  ret i32 %4, !dbg !58
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @main(i32 noundef %0, ptr noundef %1) #0 !dbg !59 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  call void @llvm.dbg.declare(metadata ptr %4, metadata !65, metadata !DIExpression()), !dbg !66
  store ptr %1, ptr %5, align 8
  call void @llvm.dbg.declare(metadata ptr %5, metadata !67, metadata !DIExpression()), !dbg !68
  %6 = call i32 @complicatedFunction(), !dbg !69
  ret i32 %6, !dbg !70
}

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5, !6, !7, !8}
!llvm.ident = !{!9}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "Debian clang version 17.0.6 (18)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/llvm/cubed.c", directory: ".", checksumkind: CSK_MD5, checksum: "9eee265bb6816adad054c09c4db471c1")
!2 = !{i32 7, !"Dwarf Version", i32 5}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !{i32 1, !"wchar_size", i32 4}
!5 = !{i32 8, !"PIC Level", i32 2}
!6 = !{i32 7, !"PIE Level", i32 2}
!7 = !{i32 7, !"uwtable", i32 2}
!8 = !{i32 7, !"frame-pointer", i32 2}
!9 = !{!"Debian clang version 17.0.6 (18)"}
!10 = distinct !DISubprogram(name: "cubed", scope: !1, file: !1, line: 24, type: !11, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !14)
!11 = !DISubroutineType(types: !12)
!12 = !{!13, !13}
!13 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!14 = !{}
!15 = !DILocalVariable(name: "n", arg: 1, scope: !10, file: !1, line: 24, type: !13)
!16 = !DILocation(line: 24, column: 15, scope: !10)
!17 = !DILocalVariable(name: "i", scope: !10, file: !1, line: 25, type: !13)
!18 = !DILocation(line: 25, column: 9, scope: !10)
!19 = !DILocalVariable(name: "res", scope: !10, file: !1, line: 26, type: !13)
!20 = !DILocation(line: 26, column: 9, scope: !10)
!21 = !DILocation(line: 27, column: 5, scope: !10)
!22 = !DILocation(line: 28, column: 5, scope: !10)
!23 = !DILocation(line: 29, column: 9, scope: !24)
!24 = distinct !DILexicalBlock(scope: !10, file: !1, line: 28, column: 26)
!25 = !DILocation(line: 30, column: 13, scope: !26)
!26 = distinct !DILexicalBlock(scope: !24, file: !1, line: 30, column: 13)
!27 = !DILocation(line: 30, column: 18, scope: !26)
!28 = !DILocation(line: 30, column: 15, scope: !26)
!29 = !DILocation(line: 30, column: 13, scope: !24)
!30 = !DILocation(line: 31, column: 13, scope: !31)
!31 = distinct !DILexicalBlock(scope: !26, file: !1, line: 30, column: 21)
!32 = !DILocation(line: 33, column: 15, scope: !24)
!33 = !DILocation(line: 33, column: 21, scope: !24)
!34 = !DILocation(line: 33, column: 25, scope: !24)
!35 = !DILocation(line: 33, column: 23, scope: !24)
!36 = !DILocation(line: 33, column: 19, scope: !24)
!37 = !DILocation(line: 33, column: 13, scope: !24)
!38 = !DILocation(line: 34, column: 13, scope: !24)
!39 = !DILocation(line: 34, column: 15, scope: !24)
!40 = !DILocation(line: 34, column: 11, scope: !24)
!41 = !DILocation(line: 35, column: 9, scope: !24)
!42 = distinct !{!42, !22, !43}
!43 = !DILocation(line: 36, column: 5, scope: !10)
!44 = !DILocation(line: 37, column: 12, scope: !10)
!45 = !DILocation(line: 37, column: 5, scope: !10)
!46 = distinct !DISubprogram(name: "complicatedFunction", scope: !1, file: !1, line: 47, type: !47, scopeLine: 47, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !14)
!47 = !DISubroutineType(types: !48)
!48 = !{!13}
!49 = !DILocalVariable(name: "a", scope: !46, file: !1, line: 48, type: !50)
!50 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "TestStruct", file: !1, line: 41, size: 64, elements: !51)
!51 = !{!52, !53}
!52 = !DIDerivedType(tag: DW_TAG_member, name: "a", scope: !50, file: !1, line: 42, baseType: !13, size: 32)
!53 = !DIDerivedType(tag: DW_TAG_member, name: "b", scope: !50, file: !1, line: 43, baseType: !13, size: 32, offset: 32)
!54 = !DILocation(line: 48, column: 23, scope: !46)
!55 = !DILocation(line: 49, column: 7, scope: !46)
!56 = !DILocation(line: 49, column: 9, scope: !46)
!57 = !DILocation(line: 50, column: 14, scope: !46)
!58 = !DILocation(line: 50, column: 5, scope: !46)
!59 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 54, type: !60, scopeLine: 54, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !14)
!60 = !DISubroutineType(types: !61)
!61 = !{!13, !13, !62}
!62 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !63, size: 64)
!63 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !64, size: 64)
!64 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!65 = !DILocalVariable(name: "argc", arg: 1, scope: !59, file: !1, line: 54, type: !13)
!66 = !DILocation(line: 54, column: 14, scope: !59)
!67 = !DILocalVariable(name: "argv", arg: 2, scope: !59, file: !1, line: 54, type: !62)
!68 = !DILocation(line: 54, column: 27, scope: !59)
!69 = !DILocation(line: 55, column: 12, scope: !59)
!70 = !DILocation(line: 55, column: 5, scope: !59)
