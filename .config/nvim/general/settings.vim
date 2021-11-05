syntax  enable                          " Enables syntax highlighing

set     hidden                          " Required to keep multiple buffers open multiple buffers

set     clipboard=unnamed,unnamedplus   " Copy paste between vim and everything else

set     encoding=utf-8                  " The encoding displayed
set     fileencoding=utf-8              " The encoding written to file
set     fileformat=unix                 " Unix file format
set     t_Co=256                        " Support 256 colors

set     tabstop=4                       " Insert 2 spaces for a tab
set     softtabstop=4			 
set     shiftwidth=4                    " Change the number of space characters inserted for indentation
set     smarttab                        " Makes tabbing smarter will realize you have 2 vs 4
set     expandtab                       " Converts tabs to spaces
set     autoindent			            " Good auto indent
set     smartindent                     " Makes indenting smart

set     formatoptions-=cro              " Stop newline continution of comments
set     conceallevel=0                  " So that I can see `` in markdown files

set     mouse=a				            " Enable your mouse

set     laststatus=0                    " Always display the status line
set     number relativenumber		    " Line numbers
set     ruler              			    " Show the cursor position all the time
set     pumheight=10                    " Makes popup menu smaller
set     cmdheight=2                     " More space for displaying messages
set     iskeyword+=-                    " treat dash separated words as a word text object"

set     splitbelow                      " Horizontal splits will automatically be below
set     splitright                      " Vertical splits will automatically be to the right

set     nobackup                        " This is recommended by coc
set     nowritebackup                   " This is recommended by coc
set     updatetime=300                  " Faster completion
set     timeoutlen=500                  " By default timeoutlen is 1000 ms

let g:loaded_ruby_provider = 0          " Disable neovim ruby provider
let g:loaded_perl_provider = 0          " Disabel neovim perl provider
