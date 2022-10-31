syn keyword pvlBool true false
syn keyword pvlNull null
syn keyword pvlThis this
syn match pvlNumber display "[0-9]+"

syn keyword pvlRepeat while for
syn keyword pvlConditional if else goto

syn keyword pvlStructure class nextgroup=pvlIdentifier skipwhite skipempty
syn match pvlIdentifier "[A-Za-z][a-zA-Z0-9_]*" display contained
syn match pvlOperator display "==\|!=\|&&\|||\|!\|<\|<=\|>\|>=\|+\|-\|*\|/\|\\/|++/|--\|==>\|**\|-*\|?\|:"

syn keyword pvlType int boolean option process zfrac frac set seq bag
syn keyword pvlStorage static local thread_local global
syn keyword pvlKeyword kernel inline pure with then in id new unfolding return lock unlock wait notify fork join barrier par and vec void

syn cluster pvlSpecInner contains=pvlSpecKeyword,pvlSpecKeywordSpecial,pvlSpecQuantifier
syn keyword pvlSpecKeyword int boolean option process zfrac frac set seq bag modifies accessible requires ensures given yields invariant context loop_invariant create qed apply use create destroy split merge choose fold unfold open close assert assume inhale exhale label refute witness ghost send recv transfer csl_subject spec_ignore null Perm HPerm empty write false true Future AbstractState to from action atomic PointsTo Value contained
syn match pvlSpecKeywordSpecial "\\result\|\\old" display contained
syn match pvlSpecQuantifier "(" skipwhite nextgroup=pvlSpecQuantifierKeyword contained
syn match pvlSpecQuantifierKeyword "\\forall\|\\exists" contained skipwhite nextgroup=pvlSpecQuantifierBody

syn match pvlSpecQuantifierBody "\_[^;]*;\_[^;]*;\_[^)]*)" contained contains=@pvlSpecInner
syn region pvlSpec matchgroup=pvlSpecStartKeyword start="resource\|modifies\|accessible\|requires\|ensures\|given\|yields\|invariant\|context_everywhere\|context\|loop_invariant\|create\|qed\|apply\|use\|create\|destroy\|split\|merge\|choose\|fold\|unfold\|open\|close\|assert\|assume\|inhale\|exhale\|label\|refute\|witness\|ghost\|send\|recv\|transfer\|csl_subject\|spec_ignore" end=";" contains=@pvlSpecInner

syn region pvlSpecParens matchgroup=pvlSpecStartKeyword start="action *(\|atomic *(" end=")" contains=@pvlSpecInner
syn region pvlCommentBlock start="/\*" end="\*/"
syn region pvlCommentLine start="//" end="$"

hi def link pvlCommentBlock pvlComment
hi def link pvlCommentLine pvlComment

hi def link pvlBool pvlConstant
hi def link pvlNull pvlConstant
hi def link pvlThis pvlConstant

hi def link pvlComment Comment

hi def link pvlConstant Constant
hi def link pvlNumber Number
hi def link pvlRepeat Repeat
hi def link pvlConditional Conditional
hi def link pvlStructure Structure
hi def link pvlIdentifier Identifier
hi def link pvlOperator Operator
hi def link pvlType Type
hi def link pvlStorage StorageClass
hi def link pvlKeyword Keyword
hi def link pvlSpecStartKeyword Special
hi def link pvlSpec SpecialComment
hi def link pvlSpecKeyword Keyword
hi def link pvlSpecKeywordSpecial Macro
hi def link pvlSpecQuantifierKeyword Macro
