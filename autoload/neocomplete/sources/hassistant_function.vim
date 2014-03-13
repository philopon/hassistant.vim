" hassistant_language
" Version: 0.0.1
" Author: 
" License: 

if exists('g:loaded_hassistant_function')
  finish
endif
let g:loaded_hassistant_function = 1

let s:save_cpo = &cpo
set cpo&vim

let s:source = {
      \ 'name' : 'hassistant_function',
      \ 'kind' : 'manual',
      \ 'rank' : 200,
      \ 'filetypes' : { 'haskell': 1 },
      \ 'hooks' : {},
      \ 'min_pattern_length': 1,
      \}

function! s:source.get_complete_position(context) "{{{
  return neocomplete#helper#match_word(a:context.input)[0]
endfunction "}}}

function! s:source.gather_candidates(context) "{{{
  call hassistant#cache_functions()
  return deepcopy(b:hassistant_functions_cache)
endfunction "}}}

function! neocomplete#sources#hassistant_function#define() "{{{
  return s:source
endfunction "}}}


let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
