" UTF-8 support
set encoding=utf-8

" spacing & indentation
set tabstop=4
set softtabstop=4
set expandtab
set autoindent

" language-specific indentation
autocmd BufRead,BufNewFile   *.rb set softtabstop=2
autocmd BufRead,BufNewFile   *.go set noexpandtab
autocmd BufRead,BufNewFile   *.hal,*.json set syntax=javascript


" leaders
let mapleader=","
let maplocalleader="\\"

" wrapping
set nowrap

" search
set ignorecase
set smartcase

" line numbers
set number

" coloring
syntax on
set hlsearch
set cursorline
highlight CursorLine cterm=NONE ctermbg=234

" mappings
map ; :
map <leader>b :buffers<CR>
map <leader>c :Clam<space> 
imap kj <Esc>


" if pathogen is there, execute it
if filereadable(expand("~/.vim/autoload/pathogen.vim"))
  " plugins currently used: clam.vim nerdtree vim-fugitive vim-go
  execute pathogen#infect()
endif


" fuzzy search using selecta(1)
" Run a given vim command on the results of fuzzy selecting from a given shell
" command. See usage below.
function! SelectaCommand(choice_command, selecta_args, vim_command)
  try
    let selection = system(a:choice_command . " | selecta " . a:selecta_args)
  catch /Vim:Interrupt/
    " Swallow the ^C so that the redraw below happens; otherwise there will be
    " leftovers from selecta on the screen
    redraw!
    return
  endtry
  redraw!
  exec a:vim_command . " " . selection
endfunction

if has("gui_running")
  " GUI stuff (GVim, MacVim, etc.)
  colors hybrid
  set lines=75
  set guioptions-=T
  set cursorline
  set guifont=Inconsolata:h14

  " copy filename of current buffer into system clipboard
  map <leader>F :let @+ = expand("%:p")<cr>
  map <leader>f :let @+ = expand("%")<cr>

  " resize Vsplits on window resize (vimbits)
  au VimResized * exe "normal! \<c-w>="
else
  " Find all files in all non-dot directories starting in the working directory.
  " Fuzzy select one of those. Open the selected file with :e.
  nnoremap <leader>t :call SelectaCommand("find * -type f", "", ":e")<cr>
endif

