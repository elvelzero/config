" Better window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <Leader>o o<Esc>^Da
nnoremap <Leader>O O<Esc>^Da

" Use alt + hjkl to resize windows
nnoremap <M-j>    :resize -2<CR>
nnoremap <M-k>    :resize +2<CR>
nnoremap <M-h>    :vertical resize -2<CR>
nnoremap <M-l>    :vertical resize +2<CR>


" Working with tab
nnoremap <silent> <C-t> :tabnew<CR>
nnoremap <silent> <M-t> :tabclose<CR>
nmap     <silent> <S-j> :tabp<CR>
nmap     <silent> <S-k> :tabn<CR>

" Alternate way to save
nnoremap <C-s> :w<CR>
" Alternate way to quit
nnoremap <C-Q> :wq!<CR>
" Use control-c instead of escape
nnoremap <C-c> <Esc>
" <TAB>: completion.
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

" Better tabbing
vnoremap < <gv
vnoremap > >gv

" Easy CAPS
inoremap <C-u> <ESC>viwUi
nnoremap <C-u> viwU<Esc>

" TAB in general mode will move to text buffer
nnoremap <TAB> :bnext<CR>
" SHIFT-TAB will go back
nnoremap <S-TAB> :bprevious<CR>

" Change working directory to current file directory
nnoremap <silent> <M-d> :lcd %:p:h<CR>

" Compile latex file (+preview when zathura open), open zathura to view compiled latex file
nnoremap <silent> <M-c> :lcd %:p:h<CR> :! pdflatex %<CR><CR>
nnoremap <silent> <M-v> :lcd %:p:h<CR> :! zathura $(echo % \| sed 's/tex$/pdf/') & disown<CR><CR>
