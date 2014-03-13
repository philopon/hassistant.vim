" hassistant
" Version: 0.0.1
" Author: 
" License: 

if exists('g:loaded_hassistant_autoload')
  finish
endif
let g:loaded_hassistant_autoload = 1

let s:save_cpo = &cpo
set cpo&vim

function! hassistant#get_language_pragma_and_imports () "{{{
  let l:lnum    = 1
  let l:pragmas = []
  let l:imports = []
  while l:lnum < line('$')
    let l:line = getline(l:lnum)
    if l:line =~# '\C^\s*{-#\s*LANGUAGE'
      call add(l:pragmas, l:line[matchend(l:line, '\C^\s*{-#\s*LANGUAGE\s*'):match(l:line, '\s*#-}') - 1])
    endif
    if l:line =~# '\C^import '
      call add(l:imports, l:line)
    endif
    let l:lnum += 1
  endwhile
  return [join(l:pragmas, ','), join(imports, "\n")]
endfunction "}}}

function! hassistant#mk_query()
  let [l:pragmas, l:imports] = hassistant#get_language_pragma_and_imports()
  return expand('%:p') . "\n" . l:pragmas . "\n" . l:imports
endfunction

function! hassistant#cache_functions()
  let l:pandi   = hassistant#mk_query()
  let l:curHash = sha256(l:pandi)

  if !exists('b:hassistant_functions_cache_hash') || b:hassistant_functions_cache_hash != l:curHash
    let b:hassistant_functions_cache = eval(libcall(g:hassistant_library, "listFunctions", l:pandi))
  endif

  if !exists('b:hassistant_functions_cache_hash')
    let b:hassistant_functions_cache_hash = sha256(l:pandi)
  endif
endfunction

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et: