vim.api.nvim_exec(
  [[
augroup global_setup_autocommands
    autocmd!

" Remember last cursor position when file loaded
autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif 

" Setup vim-markdown-folding
" autocmd BufReadPost * if @%autocmd FileType markdown set foldexpr=NestedMarkdownFolds()

" Setup Go automatic formatting
autocmd BufWritePre *.go :silent! lua require('go.format').gofmt()

augroup END
]],
  false
)
