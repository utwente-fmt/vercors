; ModuleID = 'examples/concepts/c/void.c'
source_filename = "examples/concepts/c/void.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @gt10(ptr noundef %0, i1 noundef zeroext %1) #0 !dbg !16 {
  %3 = alloca i1, align 1
  %4 = alloca ptr, align 8
  %5 = alloca i8, align 1
  %6 = alloca float, align 4
  %7 = alloca i32, align 4
  store ptr %0, ptr %4, align 8
  call void @llvm.dbg.declare(metadata ptr %4, metadata !21, metadata !DIExpression()), !dbg !22
  %8 = zext i1 %1 to i8
  store i8 %8, ptr %5, align 1
  call void @llvm.dbg.declare(metadata ptr %5, metadata !23, metadata !DIExpression()), !dbg !24
  %9 = load i8, ptr %5, align 1, !dbg !25
  %10 = trunc i8 %9 to i1, !dbg !25
  br i1 %10, label %11, label %17, !dbg !27

11:                                               ; preds = %2
  call void @llvm.dbg.declare(metadata ptr %6, metadata !28, metadata !DIExpression()), !dbg !30
  %12 = load ptr, ptr %4, align 8, !dbg !31
  %13 = load float, ptr %12, align 4, !dbg !32
  store float %13, ptr %6, align 4, !dbg !30
  %14 = load float, ptr %6, align 4, !dbg !33
  %15 = fpext float %14 to double, !dbg !33
  %16 = fcmp ogt double %15, 1.000000e+01, !dbg !34
  store i1 %16, ptr %3, align 1, !dbg !35
  br label %22, !dbg !35

17:                                               ; preds = %2
  call void @llvm.dbg.declare(metadata ptr %7, metadata !36, metadata !DIExpression()), !dbg !38
  %18 = load ptr, ptr %4, align 8, !dbg !39
  %19 = load i32, ptr %18, align 4, !dbg !40
  store i32 %19, ptr %7, align 4, !dbg !38
  %20 = load i32, ptr %7, align 4, !dbg !41
  %21 = icmp sgt i32 %20, 10, !dbg !42
  store i1 %21, ptr %3, align 1, !dbg !43
  br label %22, !dbg !43

22:                                               ; preds = %17, %11
  %23 = load i1, ptr %3, align 1, !dbg !44
  ret i1 %23, !dbg !44
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @useInt(i32 noundef %0) #0 !dbg !45 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !48, metadata !DIExpression()), !dbg !49
  %3 = call zeroext i1 @gt10(ptr noundef %2, i1 noundef zeroext false), !dbg !50
  ret i1 %3, !dbg !51
}

; Function Attrs: noinline nounwind uwtable
define dso_local zeroext i1 @useFloat(float noundef %0) #0 !dbg !52 {
  %2 = alloca float, align 4
  store float %0, ptr %2, align 4
  call void @llvm.dbg.declare(metadata ptr %2, metadata !55, metadata !DIExpression()), !dbg !56
  %3 = call zeroext i1 @gt10(ptr noundef %2, i1 noundef zeroext true), !dbg !57
  ret i1 %3, !dbg !58
}

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!8, !9, !10, !11, !12, !13, !14}
!llvm.ident = !{!15}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "Debian clang version 17.0.6 (18)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "examples/concepts/c/void.c", directory: ".", checksumkind: CSK_MD5, checksum: "162caee1ac6df37d38bd7a32756623cd")
!2 = !{!3, !5, !7}
!3 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !4, size: 64)
!4 = !DIBasicType(name: "float", size: 32, encoding: DW_ATE_float)
!5 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !6, size: 64)
!6 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!7 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!8 = !{i32 7, !"Dwarf Version", i32 5}
!9 = !{i32 2, !"Debug Info Version", i32 3}
!10 = !{i32 1, !"wchar_size", i32 4}
!11 = !{i32 8, !"PIC Level", i32 2}
!12 = !{i32 7, !"PIE Level", i32 2}
!13 = !{i32 7, !"uwtable", i32 2}
!14 = !{i32 7, !"frame-pointer", i32 2}
!15 = !{!"Debian clang version 17.0.6 (18)"}
!16 = distinct !DISubprogram(name: "gt10", scope: !1, file: !1, line: 8, type: !17, scopeLine: 8, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!17 = !DISubroutineType(types: !18)
!18 = !{!19, !7, !19}
!19 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
!20 = !{}
!21 = !DILocalVariable(name: "ptr", arg: 1, scope: !16, file: !1, line: 8, type: !7)
!22 = !DILocation(line: 8, column: 17, scope: !16)
!23 = !DILocalVariable(name: "is_float", arg: 2, scope: !16, file: !1, line: 8, type: !19)
!24 = !DILocation(line: 8, column: 27, scope: !16)
!25 = !DILocation(line: 9, column: 9, scope: !26)
!26 = distinct !DILexicalBlock(scope: !16, file: !1, line: 9, column: 9)
!27 = !DILocation(line: 9, column: 9, scope: !16)
!28 = !DILocalVariable(name: "f", scope: !29, file: !1, line: 10, type: !4)
!29 = distinct !DILexicalBlock(scope: !26, file: !1, line: 9, column: 19)
!30 = !DILocation(line: 10, column: 15, scope: !29)
!31 = !DILocation(line: 10, column: 29, scope: !29)
!32 = !DILocation(line: 10, column: 19, scope: !29)
!33 = !DILocation(line: 11, column: 16, scope: !29)
!34 = !DILocation(line: 11, column: 18, scope: !29)
!35 = !DILocation(line: 11, column: 9, scope: !29)
!36 = !DILocalVariable(name: "i", scope: !37, file: !1, line: 13, type: !6)
!37 = distinct !DILexicalBlock(scope: !26, file: !1, line: 12, column: 12)
!38 = !DILocation(line: 13, column: 13, scope: !37)
!39 = !DILocation(line: 13, column: 25, scope: !37)
!40 = !DILocation(line: 13, column: 17, scope: !37)
!41 = !DILocation(line: 14, column: 16, scope: !37)
!42 = !DILocation(line: 14, column: 18, scope: !37)
!43 = !DILocation(line: 14, column: 9, scope: !37)
!44 = !DILocation(line: 16, column: 1, scope: !16)
!45 = distinct !DISubprogram(name: "useInt", scope: !1, file: !1, line: 19, type: !46, scopeLine: 19, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!46 = !DISubroutineType(types: !47)
!47 = !{!19, !6}
!48 = !DILocalVariable(name: "a", arg: 1, scope: !45, file: !1, line: 19, type: !6)
!49 = !DILocation(line: 19, column: 17, scope: !45)
!50 = !DILocation(line: 20, column: 12, scope: !45)
!51 = !DILocation(line: 20, column: 5, scope: !45)
!52 = distinct !DISubprogram(name: "useFloat", scope: !1, file: !1, line: 24, type: !53, scopeLine: 24, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !20)
!53 = !DISubroutineType(types: !54)
!54 = !{!19, !4}
!55 = !DILocalVariable(name: "a", arg: 1, scope: !52, file: !1, line: 24, type: !4)
!56 = !DILocation(line: 24, column: 21, scope: !52)
!57 = !DILocation(line: 25, column: 12, scope: !52)
!58 = !DILocation(line: 25, column: 5, scope: !52)
