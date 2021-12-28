vim.api.nvim_exec(
  [[
augroup filetype_csv
  autocmd!

  " Format the columns more nicely when CSV buffer loads
  autocmd BufRead,BufWritePost *.csv execute ":%CSVArrangeColumn!"
  autocmd BufWritePre *.csv execute ":%CSVUnArrangeColumn"
augroup END
]],
  false
)
