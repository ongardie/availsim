if exists("b:current_syntax")
  finish
endif

syn match traceIdentifier "^.*\*$" contains=ALL

syn keyword traceKeyword       Candidate
syn keyword traceKeyword       Leader
syn match traceUnderlined    "^Tick: .*$"

" Integers.
syn match traceNumber "\<\d\+\>"

" Operators and special characters.
syn match traceOperator "[\+\-\*\/%&|=!<>:\?]\|\."
syn match traceDelimiter "\(:[^=]\|[;,]\)"
syn match traceSpecial "[()\[\]]"

" Link the rules to some groups.
highlight link traceComment        Comment
highlight link traceString         String
highlight link traceNumber         Number
highlight link traceBoolean        Boolean
highlight link traceIdentifier     Identifier
highlight link traceFunction       Function
highlight link traceStatement      Statement
highlight link traceConditional    Conditional
highlight link traceRepeat         Repeat
highlight link traceLabel          Label
highlight link traceOperator       Operator
highlight link traceKeyword        Keyword
highlight link traceType           Type
highlight link traceStructure      Structure
highlight link traceSpecial        Special
highlight link traceDelimiter      Delimiter
highlight link traceError          Error
highlight link traceTodo           Todo
highlight link traceUnderlined     Underlined

let b:current_syntax = "trace"
