" Thes were generally required in the past, but may not be anymore
set nocompatible              " be iMproved, required
filetype off                  " required

" Set leader key
let mapleader = "\<Space>"

" Determine various things about our environment
let g:is_nvim = has('nvim')
let g:is_nvim5 = has('nvim-0.5.0')
let g:is_vim = !g:is_nvim
let g:is_vim8 = v:version >= 800 && g:is_vim ? 1 : 0
let g:has_python3 = has('python3')
let g:use_coc = g:is_nvim5 || g:is_vim8
let g:has_rust_cargo = executable('cargo')
let g:is_windows = has('win32')

" +++++++++++++++++++++++++++++++ Start of Vim-Plug auto-download +++++++++++++++++++++++++++++++
" On Windows the NeoVim configuration file init.vim goes to %USERPROFILE%/AppData/Local/nvim/init.vim
" On Windows the Vim configuration file init.vim goes to %USERPROFILE%/vimfiles/vimrc
let g:xdg_config_home = $XDG_CONFIG_HOME
let g:xdg_data_home = $XDG_DATA_HOME
let g:plug_github_url = "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
let g:autoload_plug = '/autoload/plug.vim'

" Figure out where the plug-vim and plugin files go
" Note that $HOME is a somewhat magic variable that is fixed up on Windows for
" us
if g:is_vim
    if has('win32')
        let g:plug_location = $HOME . '/vimfiles'
        let g:plugin_location=''
    else
        let g:plug_location = $HOME . '/.vim'
        let g:plugin_location = ''
    endif
else
    " For Neovim here, we also take the XDG locations into account if they are
    " defined
    if has('win32')
        let g:plug_location = $HOME . '/AppData/Local/nvim/vimfiles'
        let g:plugin_location = ''
    else
        if len(g:xdg_data_home) == 0
            let g:plug_location = $HOME . '/.local/share/nvim/site'
        else
            let g:plug_location= g:xdg_data_home . '/nvim/site'
        endif
        if len(g:xdg_data_home) == 0
            let g:plugin_location = $HOME . '/.local/shared/nvim/plugged'
        else
            let g:plugin_location = g:xdg_data_home . '/nvim/plugged'
        endif
    endif
endif

" Download Plug plugin for (Neo)vim
if empty(glob(g:plug_location . g:autoload_plug))
    execute '!curl -fLo ' . g:plug_location . g:autoload_plug . ' --create-dirs ' . g:plug_github_url
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif


" If our plugin location is defined, then call plug#begin with it
if len(g:plugin_location) == 0
    call plug#begin()
else
    call plug#begin(g:plugin_location)
endif

" ========== Put your plugins here ==========


" ========== Put your plugins here ==========
" Use the below to finish your plugins (uncomment)
"call plug#end()
" +++++++++++++++++++++++++++++++ End of Vim-Plug auto-download +++++++++++++++++++++++++++++++

" +++++++++++++++++++++++++++++++ Code for AS Platform dislpay Start in Airline Status Line +++++++++++++++++++++++++++++++
function GetASPlatform()
    return g:AS_PLATFORM
endfunction

function IsASPlatformProd()
    return GetASPlatform() ==# "prod"
endfunction

function IsASPlatformDefined()
    return strlen(GetASPlatform())
endfunction

function GetASPlatformFormatted()
    return "PR [" . GetASPlatform() . "]"
endfunction

function! AirlineInit()
    let g:AS_PLATFORM=$AS_PLATFORM

    if IsASPlatformDefined()
        call airline#parts#define_function('ASPlatform', 'GetASPlatformFormatted')

        if IsASPlatformProd()
            call airline#parts#define_accent('ASPlatform', 'red')
        endif

        let g:airline_section_y = airline#section#create_right(['ffenc','ASPlatform'])
    endif

endfunction

augroup airline_init
  autocmd! *
  autocmd User AirlineAfterInit call AirlineInit()
augroup END
" +++++++++++++++++++++++++++++++ Code for AS Platform dislpay End in Airline Status Line +++++++++++++++++++++++++++++++

" +++++++++++++++ Treesitter Specific Stuff +++++++++++++++
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update

" +++++++++++++++ Treesitter Specific Stuff +++++++++++++++

" +++++++++++++++ Rust Specific Stuff +++++++++++++++

" Collection of common configurations for the Nvim LSP client
Plug 'neovim/nvim-lspconfig'

" Completion framework
Plug 'hrsh7th/nvim-cmp'

" LSP completion source for nvim-cmp
Plug 'hrsh7th/cmp-nvim-lsp'

" Snippet completion source for nvim-cmp
Plug 'hrsh7th/cmp-vsnip'

" Other usefull completion sources
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-buffer'

" See hrsh7th's other plugins for more completion sources!

" To enable more of the features of rust-analyzer, such as inlay hints and more!
Plug 'simrat39/rust-tools.nvim'

" Snippet engine
Plug 'hrsh7th/vim-vsnip'

" Telescope Stuff
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
Plug 'nvim-telescope/telescope.nvim'
Plug 'jvgrootveld/telescope-zoxide'

" +++++++++++++++ Rust Specific Stuff +++++++++++++++

" +++++++++++++++ Trouble Specific Stuff +++++++++++++++
" Vim Script
Plug 'kyazdani42/nvim-web-devicons'
Plug 'folke/trouble.nvim'
" +++++++++++++++ Trouble Specific Stuff +++++++++++++++

" +++++++++++++++ Neoclip Specific Stuff +++++++++++++++
Plug 'tami5/sqlite.lua'
Plug 'AckslD/nvim-neoclip.lua'
" +++++++++++++++ Neoclip Specific Stuff +++++++++++++++

" +++++++++++++++ Wilder Specific Stuff +++++++++++++++
Plug 'romgrk/fzy-lua-native'

if has('nvim')
  Plug 'gelguy/wilder.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'gelguy/wilder.nvim'

  " To use Python remote plugin features in Vim, can be skipped
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif


" Git Gutter Plugin
Plug 'airblade/vim-gitgutter'
" Use fontawesome icons as signs
let g:gitgutter_sign_added = ''
let g:gitgutter_sign_modified = ''
let g:gitgutter_sign_removed = 'X'
let g:gitgutter_sign_removed_first_line = ''
let g:gitgutter_sign_modified_removed = ''
let g:gitgutter_override_sign_column_highlight = 1
" highlight SignColumn guibg=bg
" highlight SignColumn ctermbg=bg
" Jump between hunks
nmap <Leader>gn <Plug>(GitGutterNextHunk)  " git next
nmap <Leader>gp <Plug>(GitGutterPrevHunk)  " git previous
" Hunk-add and hunk-revert for chunk staging
nmap <Leader>ga <Plug>(GitGutterStageHunk)  " git add (chunk)
nmap <Leader>gu <Plug>(GitGutterUndoHunk)   " git undo (chunk)

" Git Plugin
Plug 'jreybert/vimagit'
" Open vimagit pane
nnoremap <leader>gs :Magit<CR>       " git status
" Push to remote
nnoremap <leader>gP :! git push<CR>  " git Push
" Enable deletion of untracked files in Magit
"let g:magit_discard_untracked_do_delete=1

" Plug 'rust-lang/rust.vim', { 'for' : 'rust' }
Plug 'Yggdroot/indentLine'
Plug 'junegunn/vim-plug'

Plug 'vim-airline/vim-airline'
" air-line
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

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
" Show commits for every source line
nnoremap <Leader>gb :Gblame<CR>  " git blame
" Open current line in the browser
nnoremap <Leader>gb :.Gbrowse<CR>
" Open visual selection in the browser
vnoremap <Leader>gb :Gbrowse<CR>
" Add the entire file to the staging area
nnoremap <Leader>gaf :Gw<CR>      " git add file

Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-vinegar'

Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-entire'
Plug 'glts/vim-textobj-comment'
Plug 'kana/vim-textobj-indent'
Plug 'jceb/vim-textobj-uri'
Plug 'thinca/vim-textobj-between'

" Gives powerful text objects and jumping based on the language you are in
Plug 'andymass/vim-matchup'

Plug 'mileszs/ack.vim'

" Gists
Plug 'mattn/webapi-vim'
Plug 'mattn/vim-gist'

" Motion plugins
Plug 'easymotion/vim-easymotion'
" Plug 'justinmk/vim-sneak'
Plug 'unblevable/quick-scope'


Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Plug 'masukomi/vim-markdown-folding', { 'for' : 'markdown' }
" Very powerful text objects plugin
Plug 'wellle/targets.vim'

Plug 'tommcdo/vim-exchange'
Plug 'jeffkreeftmeijer/vim-numbertoggle'
" Jump to line number preview
Plug 'nacro90/numb.nvim'


" Plug 'pedrohdz/vim-yaml-folds'
Plug 'vimwiki/vimwiki'
Plug 'mattn/calendar-vim'

" For handling marks
Plug 'kshenoy/vim-signature'

" For handling root files
Plug 'lambdalisue/suda.vim'

" Lua handling
" Plug 'euclidianAce/BetterLua.vim'

if g:has_python3
    "Mundo requires python >=2.4, but we only use Python3
    Plug 'simnalamburt/vim-mundo'
endif

" Plugin to handle CSV format files
Plug 'chrisbra/csv.vim'

" Improved incremental search
Plug 'haya14busa/incsearch.vim'
Plug 'haya14busa/incsearch-easymotion.vim'

" Vim Sensible
Plug 'tpope/vim-sensible'

" Vim Diff tools
Plug 'whiteinge/diffconflicts'

" Comment colouring plugins
" Plug 'jbgutierrez/vim-better-comments'

" Colour Schemes
" - Color support plugins
" Clap (which is loaded) also has colorscheme switching
"Plug 'xolox/vim-colorscheme-switcher'
"Plug 'xolox/vim-misc'
"Plug 'vim-scripts/colorsupport.vim'
" Plug 'tanvirtin/monokai.nvim'
" Plug 'tomasr/molokai'
" Plug 'changyuheng/color-scheme-holokai-for-vim'
" Plug 'jacoborus/tender.vim'
" Plug 'bratpeki/truedark-vim'
" Plug 'romainl/vim-dichromatic'
Plug 'humanoid-colors/vim-humanoid-colorscheme'
" Plug 'tpope/vim-vividchalk'
" Plug 'ParamagicDev/vim-medic_chalk'
" Plug 'josegamez82/starrynight'
" Plug 'lewis6991/moonlight.vim'
Plug 'nanotech/jellybeans.vim'

" - Color plugins
" Plug 'cseelus/vim-colors-lucid'
" Plug 'NLKNguyen/papercolor-theme'

" Plug 'arcticicestudio/nord-vim'
" Plug 'rakr/vim-one'
" Plug 'challenger-deep-theme/vim'
" Plug 'sonph/onehalf', { 'rtp': 'vim' }

"This is for you Noah
" Plug 'dracula/vim', { 'as': 'dracula' }

call plug#end()

" Color Scheme setup

set cursorline

if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

if g:is_nvim
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

":colorscheme one
":set background=dark

":colorscheme challenger_deep
":set background=dark

":colorscheme humanoid
":set background=dark
":set background=light

" If we don't have enough colors (like I'm connecting via JuiceSSH), then just
" use the default colorscheme
if &t_Co >= 256 && ($TERM isnot# 'xterm')
" gVim would have 'gui_running'
" We silently ignore failures on the colorscheme, this is because they may not
" be loaded yet when (Neo)Vim first loads the configuration
    if has("gui_running")
        :silent! colorscheme humanoid
        :set background=light
    else
    "    :silent! colorscheme challenger_deep
    "    :silent! colorscheme dichromatic

        " :silent! colorscheme koehler
        " :silent! colorscheme tender
        " :silent! colorscheme starrynight
        " :silent! colorscheme moonlight
        :silent! colorscheme jellybeans
        " :silent! colorscheme truedark
        " set airline theme
        " let g:airline_theme = 'tender'

        :set background=dark
    endif
else
    :silent! colorscheme default
    :set background=dark
endif

" Hey Noah, this is for you...
":colorscheme dracula
":set background=dark

" Jellybeans Setup
let g:jellybeans_overrides = {
\    'background': { 'guibg': '000000' },
\}

" ZFZ Setup
nnoremap <silent> <C-p> :Files<CR>
nnoremap <silent> <Leader>f :Rg<CR>
command! -bang -nargs=* Rg call fzf#vim#grep("rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1, {'options': '--delimiter : --nth 4..'}, <bang>0)
set grepprg=rg\ --vimgrep\ --smart-case\ --follow

" Gist setup
let g:gist_clip_command = 'xclip -selection clipboard'
let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1
let g:gist_show_privates = 1
let g:gist_post_private = 1
let g:github_user=$GIST_USER
let g:gist_token=$GIST_TOKEN

" Scrolling setup
set scrolloff=3           " Display at least 3 lines around you cursor
                          " (for scrolling)
" Useful settings
syntax on " Turn on syntax highlighing

" -- Display
set title                 " Update the title of your window or your terminal
" set number                " Display line numbers
" set ruler                 " Display cursor position
" Set marker so we know the maximum line length we should use
set colorcolumn=100

" Toggle line number display
nnoremap <F2> :let [&nu, &rnu] = [!&rnu, &nu+&rnu==1]<cr>

lua require('numb').setup()

" set guioptions=T          " Enable the toolbar

" -- Search
set ignorecase            " Ignore case when searching
set smartcase             " If there is an uppercase in your search term
                          " search case sensitive again
set incsearch             " Highlight search results when typing
set hlsearch              " Highlight search results

set nowrapscan            " Don't wrap around, use ggn to start over

" -- Beep
set visualbell            " Prevent Vim from beeping
set noerrorbells          " Prevent Vim from beeping

" Backspace behaves as expected
set backspace=indent,eol,start

" Hide buffer (file) instead of abandoning when switching
" to another buffer
set hidden

" Folding settings
set foldlevelstart=20
autocmd BufWinEnter * silent! :%foldopen!
let g:indentLine_char = '⦙'

" Handle indenting things
set expandtab       "Use softtabstop spaces instead of tab characters for indentation
set shiftwidth=4    "Indent by 4 spaces when using >>, <<, == etc.
set softtabstop=4   "Indent by 4 spaces when pressing <TAB>
set tabstop=8       " Tabstops need to be 8 to keep things sane

set autoindent      "Keep indentation from previous line
set smartindent     "Automatically inserts indentation in some cases
set cindent         "Like smartindent, but stricter and more customisable

" Better word wrappping
let s:wrapenabled = 0
function! ToggleWrap()
  set wrap nolist
  if s:wrapenabled
    set nolinebreak
    unmap j
    unmap k
    unmap 0
    unmap ^
    unmap $
    let s:wrapenabled = 0
  else
    set linebreak
    nnoremap j gj
    nnoremap k gk
    nnoremap 0 g0
    nnoremap ^ g^
    nnoremap $ g$
    vnoremap j gj
    vnoremap k gk
    vnoremap 0 g0
    vnoremap ^ g^
    vnoremap $ g$
    let s:wrapenabled = 1
  endif
endfunction
map <leader>w :call ToggleWrap()<CR>

" toggle show whitespace
noremap <F3> :set list!<CR>

" :w!! to save with sudo
" ca w!! w !sudo -S tee >/dev/null "%"
" Suda plugin, which replaces the above
let g:suda_smart_edit = 1

" Insert current date or date and time in insert mode
iabbrev <silent> dst <C-R>=strftime("%Y-%m-%d")<cr>
iabbrev <silent> dstm <C-R>=strftime("%Y-%m-%d, %H:%M:%S ")<cr>

" Insert current filename
iabbrev <silent> crr <C-R>=expand('%:t:r')<cr>

" Insert result of external command
function! ExternalCommandResult()
  return system(input('Command: '))[:-2]
endfunction
inoremap <C-R>! <C-R>=ExternalCommandResult()<cr>

" Another way to do <esc> Press ji
inoremap ji <Esc>

" Undo settings
set undofile " Maintain undo history between sessions
set undodir=~/.cache/nvim/undo " Set persistent undo directory
set undolevels=10000

" Improved incremental search
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" Improved incremental search - easymotion
map z/ <Plug>(incsearch-easymotion-/)
map z? <Plug>(incsearch-easymotion-?)
map zg/ <Plug>(incsearch-easymotion-stay)

" Incremential fuzzy search and easymotion
" incsearch.vim x fuzzy x vim-easymotion

function! s:config_easyfuzzymotion(...) abort
  return extend(copy({
  \   'converters': [incsearch#config#fuzzy#converter()],
  \   'modules': [incsearch#config#easymotion#module()],
  \   'keymap': {"\<CR>": '<Over>(easymotion)'},
  \   'is_expr': 0,
  \   'is_stay': 1
  \ }), get(a:, 1, {}))
endfunction

noremap <silent><expr> <Space>/ incsearch#go(<SID>config_easyfuzzymotion())

" Move current line
nnoremap <C-j> :m+<cr>
nnoremap <C-k> :m-2<cr>

" Delete trailing whitespace
noremap <leader>dtw :%s/\s\+$//e<cr>

" For tracking what is going on in Vim
function! ToggleVerbose()
    if !&verbose
        call mkdir($HOME . "/.cache/vim/logs", "p", 0700)
        set verbosefile=~/.cache/vim/logs/verbose.log
        set verbose=15
    else
        set verbose=0
        set verbosefile=
    endif
endfunction

" Create a scroll mode from Reddit
fun! Scroll_mode()  " {{{
    let keymap = {
        \ 'k': "\<C-y>", 'K': "\<C-u>",
        \ 'j': "\<C-e>", 'J': "\<C-d>",
        \ 'h': "\<C-B>", 'H': "\<C-B>",
        \ 'l': "\<C-F>", 'L': "\<C-F>"}
    while v:true
        let char = nr2char(getchar())
        if ! has_key(keymap, char) | break | endif
            execute "normal " . expand(keymap[char])
        redraw
    endw
    execute "normal M"
    echo
endfun  " }}}
nnoremap gs :call Scroll_mode()<CR>

" Spell checking, set to en_ca and turn off by default
set spell spelllang=en_ca
set nospell
nnoremap <leader>s :set spell!<cr>

function! ToggleSlash(independent) range
  let from = ''
  for lnum in range(a:firstline, a:lastline)
    let line = getline(lnum)
    let first = matchstr(line, '[/\\]')
    if !empty(first)
      if a:independent || empty(from)
        let from = first
      endif
      let opposite = (from == '/' ? '\' : '/')
      call setline(lnum, substitute(line, from, opposite, 'g'))
    endif
  endfor
endfunction
command! -bang -range ToggleSlash <line1>,<line2>call ToggleSlash(<bang>1)
noremap <silent> <F8> :ToggleSlash<CR>

" +++++++++++++++ Rust Specific Stuff +++++++++++++++
" +++++++++++++++ Trouble Specific Stuff +++++++++++++++
lua << EOF
  require("trouble").setup {
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  }
EOF
nnoremap <leader>xx <cmd>TroubleToggle<cr>
nnoremap <leader>xw <cmd>TroubleToggle lsp_workspace_diagnostics<cr>
nnoremap <leader>xd <cmd>TroubleToggle lsp_document_diagnostics<cr>
nnoremap <leader>xq <cmd>TroubleToggle quickfix<cr>
nnoremap <leader>xl <cmd>TroubleToggle loclist<cr>
nnoremap gR <cmd>TroubleToggle lsp_references<cr>

lua << EOF
local actions = require("telescope.actions")
local trouble = require("trouble.providers.telescope")

local telescope = require("telescope")

telescope.setup {
  defaults = {
    mappings = {
      i = { ["<c-t>"] = trouble.open_with_trouble },
      n = { ["<c-t>"] = trouble.open_with_trouble },
    },
  },
}
EOF

" +++++++++++++++ Trouble Specific Stuff +++++++++++++++

" +++++++++++++++ Neoclip Specific Stuff +++++++++++++++
lua << EOF

require("telescope").load_extension("neoclip")
require('neoclip').setup({
  history = 1000,
  enable_persistant_history = true,
  preview = true,
})

EOF
" +++++++++++++++ Neoclip Specific Stuff +++++++++++++++

" Set completeopt to have a better completion experience
" :help completeopt
" menuone: popup even when there's only one match
" noinsert: Do not insert text until a selection is made
" noselect: Do not select, force user to select one from the menu
set completeopt=menuone,noinsert,noselect

" Avoid showing extra messages when using completion
set shortmess+=c

" Configure LSP through rust-tools.nvim plugin.
" rust-tools will configure and enable certain LSP features for us.
" See https://github.com/simrat39/rust-tools.nvim#configuration
lua <<EOF
local nvim_lsp = require'lspconfig'

local opts = {
    tools = { -- rust-tools options
        autoSetHints = true,
        hover_with_actions = true,
        inlay_hints = {
            show_parameter_hints = false,
            parameter_hints_prefix = "",
            other_hints_prefix = "",
        },
    },

    -- all the opts to send to nvim-lspconfig
    -- these override the defaults set by rust-tools.nvim
    -- see https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md#rust_analyzer
    server = {
        -- on_attach is a callback called when the language server attachs to the buffer
        -- on_attach = on_attach,
        settings = {
            -- to enable rust-analyzer settings visit:
            -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
            ["rust-analyzer"] = {
                -- enable clippy on save
                checkOnSave = {
                    command = "clippy"
                },
            }
        }
    },
}

require('rust-tools').setup(opts)
EOF

" Setup Completion
" See https://github.com/hrsh7th/nvim-cmp#basic-configuration
lua <<EOF
local cmp = require'cmp'
cmp.setup({
  -- Enable LSP snippets
  snippet = {
    expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    -- Add tab support
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    })
  },

  -- Installed sources
  sources = {
    { name = 'nvim_lsp' },
    { name = 'vsnip' },
    { name = 'path' },
    { name = 'buffer' },
  },
})
EOF

" +++++++++++++++ Rust Specific Stuff +++++++++++++++

" +++++++++++++++ Telescope Specific Stuff +++++++++++++++
lua <<EOF
-- You dont need to set any of these options. These are the default ones. Only
-- the loading is important
require('telescope').setup {
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  }
}
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')
EOF
" +++++++++++++++ Telescope Specific Stuff +++++++++++++++

" +++++++++++++++ Zoxide Specific Stuff +++++++++++++++
lua <<EOF
vim.api.nvim_set_keymap(
	"n",
	"<leader>cd",
	":lua require'telescope'.extensions.zoxide.list{}<CR>",
	{noremap = true, silent = true}
)
EOF
" +++++++++++++++ Zoxide Specific Stuff +++++++++++++++

" +++++++++++++++ Neovim LSP Specific Stuff +++++++++++++++

" Python configuration
lua <<EOF
local python_lsp = require'lspconfig'
python_lsp.pyright.setup{}
EOF

" Bash configuration
lua <<EOF
local bash_lsp = require'lspconfig'
bash_lsp.bashls.setup{}
EOF

" Go configuration
lua <<EOF
local go_lsp = require'lspconfig'
go_lsp.gopls.setup{}
EOF

" Vim configuration
lua <<EOF
local vim_lsp = require'lspconfig'
vim_lsp.vimls.setup{}
EOF

" Yaml configuration
lua <<EOF
local yaml_lsp = require'lspconfig'
yaml_lsp.yamlls.setup{}
EOF
" +++++++++++++++ Neovim LSP Specific Stuff +++++++++++++++

" +++++++++++++++ Treesitter Stuff +++++++++++++++
lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  sync_install = false, -- install languages synchronously (only applied to `ensure_installed`)
  --ignore_install = { "javascript" }, -- List of parsers to ignore installing
  ignore_install = { }, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    -- disable = { "c", "rust" },  -- list of language that will be disabled
    disable = { },  -- list of language that will be disabled
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}
EOF

set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()

" +++++++++++++++ Treesitter Stuff +++++++++++++++

" +++++++++++++++ Wilder Specific Stuff ++++++++++++++
call wilder#setup({'modes': [':', '/', '?']})
call wilder#set_option('use_python_remote_plugin', 0)

call wilder#set_option('pipeline', [
      \   wilder#branch(
      \     wilder#cmdline_pipeline({
      \       'fuzzy': 1,
      \       'fuzzy_filter': wilder#lua_fzy_filter(),
      \     }),
      \     wilder#vim_search_pipeline(),
      \   ),
      \ ])

call wilder#set_option('renderer', wilder#renderer_mux({
      \ ':': wilder#popupmenu_renderer({
      \   'highlighter': wilder#lua_fzy_highlighter(),
      \   'left': [
      \     ' ',
      \     wilder#popupmenu_devicons(),
      \   ],
      \   'right': [
      \     ' ',
      \     wilder#popupmenu_scrollbar(),
      \   ],
      \ }),
      \ '/': wilder#wildmenu_renderer({
      \   'highlighter': wilder#lua_fzy_highlighter(),
      \ }),
      \ }))
" +++++++++++++++ Wilder Specific Stuff +++++++++++++++

" ------------------------------------------------------------------------------
" FileType mappings
augroup filetype_yaml
  autocmd!

  " Setup shifts and tabstops etc for YAML files
  autocmd FileType yaml,yml setlocal ts=2 sts=2 sw=2 expandtab indentkeys-=0# indentkeys-=<:>

" To get this effect for a single file you can add this to the top of a file:
" # vim: set shiftwidth=2 tabstop=2 softtabstop=2 expandtab:
"
" To change files that have a mix of tabs and spaces to all spaces
" In VIM's command mode enter the following
" :set shiftwidth=2 tabstop=2 softtabstop=2 expandtab
" :retab
augroup END

augroup filetype_csv
  autocmd!

  " Format the columns more nicely when CSV buffer loads
  autocmd BufRead,BufWritePost *.csv execute ":%ArrangeColumn!"
  autocmd BufWritePre *.csv execute ":%UnArrangeColumn"
augroup END

augroup filetype_perl
  autocmd!

  autocmd BufWritePost perl silent! !tidyall %:p
augroup END

augroup filetype_toml
  autocmd!

  autocmd FileType toml setlocal commentstring=#\ %s
augroup END

" " highlights yanked text
" augroup highlight_yank
"     autocmd!
"     autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank()

