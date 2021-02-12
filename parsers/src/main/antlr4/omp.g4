grammar omp;

omp_pragma
 : omp_parallel
// task generating constructs
 | omp_for
 | omp_single
 | omp_simd
 | omp_sections
// other constructs.
 | omp_section
// composite constructs.
 | omp_parallel_for
 ;

omp_parallel : 'parallel' omp_option* ;

omp_for : 'for' omp_option* ;

omp_single : 'single' omp_option* ;

omp_simd : 'simd' omp_option* ;

omp_sections : 'sections' omp_option* ;

omp_section : 'section' omp_option* ;

omp_parallel_for : 'parallel' 'for' omp_option* ;

omp_option : omp_private | omp_nowait | omp_schedule | omp_simdlen | omp_simdopt ;

omp_schedule : 'schedule' '(' 'static' ')' ;

omp_private : 'private' '(' id_list_empty ')' ;

omp_nowait : 'nowait' ;

omp_simdopt : 'simd' ;

omp_simdlen : 'simdlen' '(' Constant ')' ;
 
// id_list : ( Identifier ( ',' Identifier )* )? ;

id_list_empty : id_list | /* empty */ ;

id_list : Identifier | id_list  ',' Identifier ;

Identifier : [a-zA-Z_] ([0-9a-zA-Z_])* ;

Constant : [1-9]([0-9])* ;

WhiteSpace
        :   (   ' '
        |   '\t'
        |   '\r'
        |   '\n'
        )+ -> skip ;
