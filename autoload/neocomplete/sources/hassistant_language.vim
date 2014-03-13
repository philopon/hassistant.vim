" hassistant_language
" Version: 0.0.1
" Author: 
" License: 

if exists('g:loaded_hassistant_language')
  finish
endif
let g:loaded_hassistant_language = 1

let s:save_cpo = &cpo
set cpo&vim

let s:source = {
      \ 'name' : 'hassistant_language',
      \ 'kind' : 'manual',
      \ 'mark' : '[LANGUAGE]',
      \ 'rank' : 200,
      \ 'filetypes' : { 'haskell': 1 },
      \ 'hooks' : {},
      \}

function! s:source.get_complete_position(context) "{{{
  if a:context.input =~# '\s*{-#\s*LANGUAGE\s\+'
    return neocomplete#helper#match_word(a:context.input)[0]
  endif
  return -1
endfunction "}}}

function! s:source.gather_candidates(context) "{{{
  return eval(libcall(g:hassistant_library, "listLANGUAGE", ""))
endfunction "}}}

function! neocomplete#sources#hassistant_language#define() "{{{
  return s:source
endfunction "}}}


let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
