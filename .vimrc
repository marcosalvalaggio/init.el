set number
set tabstop=4
set shiftwidth=4
set expandtab
syntax on
set noerrorbells

"Copy
"vnoremap <C-c> :w !clip.exe<CR><CR>
"nnoremap <C-a> :normal! gg0vG$<CR>
"
"" Shortcuts
let mapleader = " "
nnoremap <leader>e :Ex<CR>
nnoremap <leader>q :q!<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>h :nohlsearch<CR>
nnoremap <leader>i >>
vnoremap <leader>i >gv
nnoremap <leader>u <<
vnoremap <leader>u <gv
