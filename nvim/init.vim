" Specify a directory for plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'

Plug 'joshdick/onedark.vim'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'ctrlpvim/ctrlp.vim'

Plug 'majutsushi/tagbar'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'

Plug 'airblade/vim-gitgutter'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'slashmili/alchemist.vim'
Plug 'elixir-editors/vim-elixir'
Plug 'vim-erlang/vim-erlang-runtime'

call plug#end()

map <C-n> :NERDTreeToggle<CR>

" onedark.vim theme
syntax enable 
colorscheme onedark

" airline customization
let g:airline#extensions#tabline#enabled = 1

" deoplete
let g:deoplete#enable_at_startup = 1
let g:lightline = { 'colorscheme': 'onedark' }

" ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|_build\|deps'

" tagbar
nmap <F8> :TagbarToggle<CR>

" custom mappings
noremap <Leader>\t :cbottom terminal<CR>


" The minimum essentials
" These are on by default but making explicit
set nocompatible
filetype plugin on

" Nice to have
set hlsearch
set incsearch
set number

" Tab configuration
set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab


" abbreviations for personal use
abbreviate aas Altug Alp Sahin
