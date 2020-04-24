set encoding=utf-8

" Specify a directory for plugins
call plug#begin('~/.local/share/nvim/plugged')

" themes
Plug 'joshdick/onedark.vim'

Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/syntastic'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'chiel92/vim-autoformat'
Plug 'haya14busa/is.vim'
Plug 'mattn/emmet-vim'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdcommenter'

" python
Plug 'davidhalter/jedi-vim'

" languages - linteUpdateRemotePluginsr
Plug 'dense-analysis/ale'

" languages - elixir
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'slashmili/alchemist.vim'
Plug 'elixir-editors/vim-elixir'
Plug 'vim-erlang/vim-erlang-runtime'
Plug 'mhinz/vim-mix-format'

" languages - markdown
Plug 'plasticboy/vim-markdown'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install' }
Plug 'junegunn/limelight.vim' " generial editing
Plug 'godlygeek/tabular'

" languages - dart
Plug 'dart-lang/dart-vim-plugin'

" languages - js jsx ts etc...
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'yuezk/vim-js'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'maxmellon/vim-jsx-pretty'

" languages - docker
Plug 'ekalinin/dockerfile.vim'

" fzf
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'

" fugitive
Plug 'tpope/vim-fugitive'

" rpgrep
Plug 'jremmen/vim-ripgrep'

" async lint engine
Plug 'dense-analysis/ale'

call plug#end()

" onedark.vim theme
syntax enable 
colorscheme onedark


" nerdtree
map <C-n> :NERDTreeToggle<CR>
set wildignore+=*/tmp/*,*.so,*.swp,*.zip 

let NERDTreeRespectWildIgnore=1
let NERDTreeIgnore=['\.git$', '\~$']

" airline customization
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" deoplete
let g:deoplete#enable_at_startup = 1
" Set bin if you have many instalations
let g:deoplete#sources#ternjs#tern_bin = '/usr/local/bin/tern'
let g:deoplete#sources#ternjs#timeout = 1

" Whether to include the types of the completions in the result data. Default: 0
let g:deoplete#sources#ternjs#types = 1

" Whether to include the distance (in scopes for variables, in prototypes for 
" properties) between the completions and the origin position in the result 
" data. Default: 0
let g:deoplete#sources#ternjs#depths = 1

" Whether to include documentation strings (if found) in the result data.
" Default: 0
let g:deoplete#sources#ternjs#docs = 1

" When on, only completions that match the current word at the given point will
" be returned. Turn this off to get all results, so that you can filter on the 
" client side. Default: 1
let g:deoplete#sources#ternjs#filter = 0

" Whether to use a case-insensitive compare between the current word and 
" potential completions. Default 0
let g:deoplete#sources#ternjs#case_insensitive = 1

" When completing a property and no completions are found, Tern will use some 
" heuristics to try and return some properties anyway. Set this to 0 to 
" turn that off. Default: 1
let g:deoplete#sources#ternjs#guess = 0

" Determines whether the result set will be sorted. Default: 1
let g:deoplete#sources#ternjs#sort = 0

" When disabled, only the text before the given position is considered part of 
" the word. When enabled (the default), the whole variable name that the cursor
" is on will be included. Default: 1
let g:deoplete#sources#ternjs#expand_word_forward = 0

" Whether to ignore the properties of Object.prototype unless they have been 
" spelled out by at least two characters. Default: 1
let g:deoplete#sources#ternjs#omit_object_prototype = 0

" Whether to include JavaScript keywords when completing something that is not 
" a property. Default: 0
let g:deoplete#sources#ternjs#include_keywords = 1

" If completions should be returned when inside a literal. Default: 1
let g:deoplete#sources#ternjs#in_literal = 0


"Add extra filetypes
let g:deoplete#sources#ternjs#filetypes = [
                \ 'jsx',
                \ 'javascript.jsx',
                \ 'vue',
                \ ]

let g:lightline = { 'colorscheme': 'onedark' }

" ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|_build\|deps'

" tagbar
nmap <F8> :TagbarToggle<CR>


" limelight
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240

" markdown
let g:mkdp_refresh_slow=1
let g:mkdp_markdown_css='/Users/asahin/Dev/github-md.css'

autocmd FileType markdown normal zR

" dart
let dart_html_in_string=v:true
let g:dart_style_guide = 2
let g:dart_format_on_save = 1

let g:LanguageClient_serverCommands = {
 \ 'dart': ['dart_language_server'],
 \ }

" fzf
let mapleader = " "
nnoremap <silent> <Leader><Space> :Files<CR>
nmap <silent> <leader>m :History<CR>
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)


" other mappings
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
