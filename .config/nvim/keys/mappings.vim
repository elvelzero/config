" TAB in general mode will move to text buffer
nnoremap <TAB> :bnext<CR>
" SHIFT-TAB will go back
nnoremap <S-TAB> :bprevious<CR>

" Better tabbing
vnoremap < <gv
vnoremap > >gv

" Better window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <silent> <M-b> :NERDTreeToggle<CR>

" change working directory to current file directory
nnoremap <silent> <M-d> :lcd %:p:h<CR>

" tab alias
nnoremap <silent> <C-t> :tabnew<CR>
nnoremap <silent> <M-t> :tabclose<CR>
nmap     <silent> <M-j> :tabp<CR>
nmap     <silent> <M-k> :tabn<CR>

" for latex, compile latex file (+preview when zathura open), open zathura to view compiled latex file
nnoremap <silent> <M-c> :lcd %:p:h<CR> :! pdflatex %<CR><CR>
nnoremap <silent> <M-v> :lcd %:p:h<CR> :! zathura $(echo % \| sed 's/tex$/pdf/') & disown<CR><CR>
