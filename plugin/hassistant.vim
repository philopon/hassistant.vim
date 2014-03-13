" hassistant
" Version: 0.0.1
" Author: 
" License: 

if exists('g:loaded_hassistant')
  finish
endif
let g:loaded_hassistant = 1

let s:save_cpo = &cpo
set cpo&vim

if !exists('g:hassistant_library')
  let g:hassistant_library = expand('<sfile>:p:h:h') . '/dist/build/hassistant.dylib/hassistant.dylib'
endif

call neocomplete#custom#source('hassistant_function', 'converters',
      \ ['converter_remove_overlap', 'converter_case', 'converter_abbr'])

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
