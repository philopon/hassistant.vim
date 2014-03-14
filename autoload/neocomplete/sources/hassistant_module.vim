" hassistant
" Version: 0.0.1
" Author: 
" License: 

if exists('g:loaded_hassistant_module')
  finish
endif
let g:loaded_hassistant_module = 1

let s:save_cpo = &cpo
set cpo&vim

let s:source = {
      \ 'name' : 'hassistant_module',
      \ 'kind' : 'manual',
      \ 'mark' : '[Module]',
      \ 'rank' : 200,
      \ 'filetypes' : { 'haskell': 1 },
      \ 'hooks' : {},
      \}

function! s:source.get_complete_position(context) "{{{
  if a:context.input =~# '^import .*('
    let a:context.in_module_function = 1
    return neocomplete#helper#match_word(a:context.input)[0]
  elseif a:context.input =~# '^import '
    let a:context.in_module_function = 0
    return neocomplete#helper#match_word(a:context.input)[0]
  endif
  return -1
endfunction "}}}

function! s:source.gather_candidates(context) "{{{
  if a:context.in_module_function
    let l:mdl = hassistant#get_moduleName(a:context.input)
    echomsg l:mdl
    return eval(libcall(g:hassistant_library, "listNamesInModule", l:mdl))
  else 
    return eval(libcall(g:hassistant_library, "listModule", expand('%:p')))
  endif
endfunction "}}}

function! neocomplete#sources#hassistant_module#define() "{{{
  return s:source
endfunction "}}}


let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
