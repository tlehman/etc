" spacing
set tabstop=4
set softtabstop=2
set expandtab

" wrapping
set nowrap

" line numbers
set number

" coloring
syntax on
set hlsearch
set cursorline

" mappings
map ; :
imap kj <Esc>

" UTF-8 support
set encoding=utf-8

" GUI stuff (GVim, MacVim, etc.)
if has("gui_running")
  colors hybrid
  set lines=75
  set guioptions-=T
  set cursorline
  set guifont=Inconsolata:h14

  " copy filename of current buffer into system clipboard
  map ,F :let @+ = expand("%:p")<cr>
  map ,f :let @+ = expand("%")<cr>

  " resize Vsplits on window resize (vimbits)
  au VimResized * exe "normal! \<c-w>="
else
endif


