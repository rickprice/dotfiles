" Vim 9.0 Configuration
" Translated from Neovim config for plugin-free environment

" ============================================================================
" Basic Settings
" ============================================================================

set nocompatible
filetype plugin indent on
syntax on

" Encoding
set encoding=utf-8
set fileencoding=utf-8

" ============================================================================
" Editor Behavior
" ============================================================================

" Backspace behavior
set backspace=indent,eol,start

" Clipboard - use system clipboard (unnamed for X11 primary selection)
set clipboard=unnamed

" Line numbers (disabled in your config)
set nonumber
set norelativenumber

" Search settings
set hlsearch        " Highlight search results
set incsearch       " Incremental search
set ignorecase      " Case insensitive search
set smartcase       " Case sensitive when uppercase present
set nowrapscan      " Don't wrap search at end of file

" Indentation
set expandtab       " Use spaces instead of tabs
set tabstop=4       " Tab width
set shiftwidth=4    " Indent width
set smartindent     " Smart auto-indenting
set shiftround      " Round indent to multiple of shiftwidth

" Line wrapping
set wrap
set linebreak       " Break at word boundaries
set showbreak=↪\    " Character to show at start of wrapped lines

" Scrolling context
set scrolloff=4     " Keep 4 lines above/below cursor
set sidescrolloff=8 " Keep 8 columns left/right of cursor

" ============================================================================
" UI Settings
" ============================================================================

" Color column at 100 characters
set colorcolumn=100

" Cursor settings (no cursorline/cursorcolumn in your config)
set nocursorline
set nocursorcolumn

" Cursor shape for different modes (approximation of guicursor)
if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\e[5 q\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\e[2 q\<Esc>\\"
else
    let &t_SI = "\e[6 q"  " Insert mode - vertical bar
    let &t_EI = "\e[2 q"  " Normal mode - block
endif

" Show matching brackets
set showmatch

" Command line
set showcmd         " Show partial commands
set noshowmode      " Don't show mode (matches your config)

" Whitespace visualization
set list
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣

" Sign column - always show, auto expand up to 9
set signcolumn=auto:9

" Split behavior
set splitbelow      " New horizontal splits below
set splitright      " New vertical splits to right

" Mouse support
set mouse=a

" Completion menu
set completeopt=menu,menuone,noselect

" ============================================================================
" File Handling
" ============================================================================

" Hidden buffers
set hidden

" Undo settings
if has('persistent_undo')
    set undofile
    set undolevels=10000
    if has('win32') || has('win64')
        if !isdirectory($HOME . '/vimfiles/undo')
            call mkdir($HOME . '/vimfiles/undo', 'p')
        endif
        set undodir=$HOME/vimfiles/undo
    else
        if !isdirectory($HOME . '/.vim/undo')
            call mkdir($HOME . '/.vim/undo', 'p', 0700)
        endif
        set undodir=~/.vim/undo
    endif
endif

" View settings - save and restore folds, cursor position, etc.
set viewoptions=cursor,folds,slash,unix
if has('win32') || has('win64')
    if !isdirectory($HOME . '/vimfiles/view')
        call mkdir($HOME . '/vimfiles/view', 'p')
    endif
    set viewdir=$HOME/vimfiles/view
else
    if !isdirectory($HOME . '/.vim/view')
        call mkdir($HOME . '/.vim/view', 'p', 0700)
    endif
    set viewdir=~/.vim/view
endif

" Backup and swap (disable for cleaner operation)
set nobackup
set nowritebackup
set noswapfile

" Vim info file - remember marks, registers, command history, etc.
if has('viminfo')
    if has('win32') || has('win64')
        set viminfo='1000,<1000,s100,h,n$HOME/vimfiles/viminfo
    else
        set viminfo='1000,<1000,s100,h,n~/.vim/viminfo
    endif
    " '1000  - remember marks for last 1000 files
    " <1000  - save up to 1000 lines for each register
    " s100   - registers with more than 100 KB are skipped
    " h      - disable hlsearch when loading viminfo
    " n      - viminfo file path
endif

" ShaDa file for Vim 8+ (alternative to viminfo)
if has('nvim') || has('shada')
    if has('win32') || has('win64')
        set shada='1000,<1000,s100,h,n$HOME/vimfiles/shada
    else
        set shada='1000,<1000,s100,h,n~/.vim/shada
    endif
endif

" Command history
set history=10000

" ============================================================================
" Formatting
" ============================================================================

set formatoptions=l
set nojoinspaces    " Don't insert two spaces after '.', '?', '!' with join

" ============================================================================
" Folding
" ============================================================================

set foldmethod=indent
set foldlevel=99
set nofoldenable

" ============================================================================
" Terminal Settings
" ============================================================================

" Enable 256 colors
if has('termguicolors')
    set termguicolors
endif

" ============================================================================
" Autocommands
" ============================================================================

" Highlight yanked text
augroup highlight_yank
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank({higroup="IncSearch", timeout=150})
augroup END

" Restore cursor position
augroup restore_cursor
    autocmd!
    autocmd BufReadPost *
        \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
        \ |   exe "normal! g`\""
        \ | endif
augroup END

" Auto-save and restore view (folds, cursor, etc.) for all files
augroup auto_view
    autocmd!
    " Save view on buffer write or leave
    autocmd BufWinLeave,BufWritePost * if expand('%') != '' && &buftype !~# '\(quickfix\|nofile\|help\)' | silent! mkview | endif
    " Restore view on buffer read
    autocmd BufWinEnter * if expand('%') != '' && &buftype !~# '\(quickfix\|nofile\|help\)' | silent! loadview | endif
augroup END

" ============================================================================
" Netrw Settings (file explorer)
" ============================================================================

" Disable netrw (as in your nvim config)
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1

" ============================================================================
" Markdown Settings
" ============================================================================

let g:markdown_fenced_languages = ['html', 'javascript', 'typescript', 'css', 'scss', 'lua', 'vim']

" Prevent vimwiki from treating all .md files as wiki files
let g:vimwiki_global_ext = 0

" ============================================================================
" Movement Settings
" ============================================================================

" Allow arrow keys to wrap between lines in insert mode
set whichwrap+=<,>

" ============================================================================
" Key Mappings
" ============================================================================

" Set leader key
let mapleader = " "
let maplocalleader = " "

" Better window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Resize windows with arrow keys
nnoremap <C-Up> :resize +2<CR>
nnoremap <C-Down> :resize -2<CR>
nnoremap <C-Left> :vertical resize -2<CR>
nnoremap <C-Right> :vertical resize +2<CR>

" Navigate buffers
nnoremap <S-l> :bnext<CR>
nnoremap <S-h> :bprevious<CR>

" Clear search highlighting with <Esc>
nnoremap <Esc> :nohlsearch<CR><Esc>

" Better indenting in visual mode
vnoremap < <gv
vnoremap > >gv

" Move text up and down
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Paste without yanking in visual mode
vnoremap p "_dP

" Keep cursor centered when searching
nnoremap n nzzzv
nnoremap N Nzzzv

" Undo break points
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ! !<c-g>u
inoremap ? ?<c-g>u

" ============================================================================
" Color Scheme
" ============================================================================

" Use a built-in color scheme (closest to modern themes)
set background=dark
colorscheme slate

" Customize colors for better readability
highlight ColorColumn ctermbg=235 guibg=#2c2d27
highlight SignColumn ctermbg=NONE guibg=NONE
highlight LineNr ctermfg=242 guifg=#6c6c6c
highlight CursorLineNr ctermfg=yellow guifg=#ffd700
highlight Comment ctermfg=246 guifg=#949494
highlight Visual ctermbg=238 guibg=#444444

" ============================================================================
" Custom Commands
" ============================================================================

" Quick save
nnoremap <leader>w :w<CR>

" Quick quit
nnoremap <leader>q :q<CR>

" Save and quit
nnoremap <leader>x :x<CR>

" Reload vimrc
nnoremap <leader>sv :source $MYVIMRC<CR>

" Edit vimrc
nnoremap <leader>ev :edit $MYVIMRC<CR>

" ============================================================================
" Statusline (simple custom statusline)
" ============================================================================

set laststatus=2
set statusline=
set statusline+=%#PmenuSel#
set statusline+=\ %f\                    " File path
set statusline+=%#LineNr#
set statusline+=\ %m                     " Modified flag
set statusline+=\ %r                     " Readonly flag
set statusline+=%=                       " Right side
set statusline+=%#CursorColumn#
set statusline+=\ %y                     " File type
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
set statusline+=\ [%{&fileformat}\]
set statusline+=\ %p%%                   " Percentage through file
set statusline+=\ %l:%c                  " Line:Column
set statusline+=\

" ============================================================================
" Performance
" ============================================================================

set lazyredraw      " Don't redraw during macros
set updatetime=300  " Faster completion
set timeoutlen=500  " Faster key sequence completion

" ============================================================================
" Wildmenu (command completion)
" ============================================================================

set wildmenu
set wildmode=longest:full,full
if has('win32') || has('win64')
    set wildignore=*.o,*~,*.pyc,*.obj,*.exe,*.dll,*.pdb,*/.git/*,*/.hg/*,*/.svn/*
else
    set wildignore=*.o,*~,*.pyc,*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

" ============================================================================
" Built-in Features (plugin alternatives)
" ============================================================================

" Better grep - use built-in Windows findstr on Windows
if has('win32') || has('win64')
    set grepprg=findstr\ /n\ /s
    set shellslash  " Use forward slashes in paths (works better in Vim)
elseif executable('rg')
    set grepprg=rg\ --vimgrep\ --smart-case\ --follow
endif

" ============================================================================
" Additional Enhancements
" ============================================================================

" Automatically close preview window after completion
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" Remove trailing whitespace on save
autocmd BufWritePre * :%s/\s\+$//e

" Auto-format Go files
autocmd FileType go setlocal noexpandtab tabstop=4 shiftwidth=4

" YAML settings
autocmd FileType yaml,yml setlocal tabstop=2 shiftwidth=2

" JSON settings
autocmd FileType json setlocal tabstop=2 shiftwidth=2

" JavaScript/TypeScript settings
autocmd FileType javascript,typescript,javascriptreact,typescriptreact setlocal tabstop=2 shiftwidth=2

" ============================================================================
" End of Configuration
" ============================================================================
