filetype plugin indent on 
set tabstop=4
set shiftwidth=4
set expandtab
set hlsearch
set number
syntax on
set fdm=indent
set autoindent
set smartindent
set scrolloff=3

"Custom color scheme
colorscheme pocketrocket

"Custom syntax file for assembly
au BufRead,BufNewFile *.s   set filetype=asmIntel
au BufRead,BufNewFile *.asm set filetype=asmIntel

"Set rules for text files (text width, spelling, etc)
au BufRead,BufNewFile *.txt set filetype=myTxt
