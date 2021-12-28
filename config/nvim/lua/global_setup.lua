vim.api.nvim_exec(
  [[
augroup global_setup_autocommands
    autocmd!

" Remember last cursor position when file loaded
autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif 

augroup END
]],
  false
)
