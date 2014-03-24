" hassistant_language
" Version: 0.0.1
" Author: 
" License: 

if exists('g:loaded_hassistant_names')
  finish
endif
let g:loaded_hassistant_names = 1

let s:save_cpo = &cpo
set cpo&vim

let s:source = {
      \ 'name' : 'hassistant_names',
      \ 'kind' : 'manual',
      \ 'rank' : 200,
      \ 'filetypes' : { 'haskell': 1 },
      \ 'hooks' : {},
      \ 'min_pattern_length': 1,
      \}

function! s:source.hooks.on_init(context) "{{{
  call hassistant#cache_names()
  augroup hassistant_names
    autocmd! * <buffer>
    autocmd InsertLeave <buffer> call hassistant#cache_names()
  augroup END
endfunction "}}}

function! s:source.get_complete_position(context) "{{{
  if a:context.input !~# '\C\(^import \|{-#\s*LANGUAGE \)'
    return neocomplete#helper#match_word(a:context.input)[0]
  endif
  return -1
endfunction "}}}

function! s:source.gather_candidates(context) "{{{
  if synIDattr(synID(line('.'), a:context.complete_pos, 1), 'name') == 'hsType'
        \ || a:context.input =~# '[a-zA-Z0-9 '']::[a-zA-Z0-9 '']'
        \ || a:context.input =~# '\<deriving\>'
        \ || a:context.input =~# '\(\<class\>\|\<instance\>\)'
    return deepcopy(b:hassistant_types_cache)
  endif
  return deepcopy(b:hassistant_functions_cache + b:hassistant_data_cache)
endfunction "}}}

function! neocomplete#sources#hassistant_names#define() "{{{
  return s:source
endfunction "}}}


let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
