call plug#begin('~/.config/nvim/autoload/plugged')

    " Themes
    Plug 'arcticicestudio/nord-vim'                     " Nord colorscheme
    Plug 'vim-airline/vim-airline'                      " airline
    Plug 'vim-airline/vim-airline-themes'               " airline themes
    
    " Essesntial
    Plug 'preservim/nerdtree'                           " Nerdtree
    Plug 'neoclide/coc.nvim', {'branch': 'release'}     " Stable version of coc
    
    " Auto completion
    Plug 'jiangmiao/auto-pairs'                         " Auto pairs for '(' '[' '{'
    Plug 'alvan/vim-closetag'                           " Auto closetag
    Plug 'tpope/vim-surround'                           " Vim surround

    Plug 'honza/vim-snippets'                           " Vim snippets
    Plug 'lervag/vimtex'                                " Vim latex
    Plug 'SirVer/ultisnips'                             " Ultisnips
    
    " Others utilities
    Plug 'lilydjwg/colorizer'                           " Color highlighter
    Plug 'mg979/vim-visual-multi'                       " Multi cuursor
    Plug 'vim-syntastic/syntastic'                      " Syntastic

call plug#end()
