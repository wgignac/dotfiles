"Clear any existing highlighting/syntax information
highlight clear
if exists("syntax_on")
    syntax reset
endif

"Scheme info: dark background and stupid name
set background=dark
let g:colors_name="pocketrocket"

"Colors themselves
highlight LineNr        ctermfg=239
highlight String        ctermfg=23
highlight Statement     ctermfg=24
highlight Normal        ctermfg=245
highlight Type          ctermfg=29
highlight Constant      ctermfg=131
highlight Number        ctermfg=89
highlight Special       ctermfg=130
highlight PreProc       ctermfg=97
highlight Include       ctermfg=97
highlight Comment       ctermfg=239
highlight Function      ctermfg=89
highlight Todo          ctermfg=11    ctermbg=none
highlight Search        ctermfg=11    ctermbg=none
highlight Folded        ctermfg=53    ctermbg=none
highlight DiffChange    ctermfg=25    ctermbg=none
highlight DiffText      ctermfg=89    ctermbg=none    cterm=bold
highlight DiffAdd       ctermfg=130   ctermbg=none    cterm=bold
highlight DiffDelete    ctermfg=130   ctermbg=none    cterm=bold
highlight MatchParen    ctermfg=190   ctermbg=none    cterm=underline
