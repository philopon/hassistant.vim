if exists('g:loaded_hassistant')
  finish
endif
let g:loaded_hassistant = 1

let s:save_cpo = &cpo
set cpo&vim

if !exists('g:hassistant_executable_directory')
  let g:hassistant_executable_directory = expand('<sfile>:h') . '/../.cabal-sandbox/bin/'
endif
let g:hassistant_modes = eval(libcall(g:hassistant_executable_directory . "library.so", "listModes", 0))

augroup Hassistant
  autocmd!
  autocmd FileType haskell call hassistant#start_type_process()
  autocmd FileType haskell autocmd InsertLeave <buffer> call hassistant#start_type_process()
  autocmd FileType haskell autocmd CursorMoved,CursorMovedI <buffer> echo hassistant#get_type(expand('<cword>'))
augroup END

let &cpo = s:save_cpo
unlet s:save_cpo
