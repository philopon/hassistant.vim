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

function! hassistant#mk_query() "{{{
  let [l:pragmas, l:imports] = hassistant#get_language_pragma_and_imports()
  return expand('%:p') . "\n" . l:pragmas . "\n" . l:imports
endfunction "}}}

function! hassistant#cache_functions() "{{{
  if !exists('b:hassistant_cache_hash')
    let l:pandi   = hassistant#mk_query()
    let [b:hassistant_functions_cache, b:hassistant_types_cache] = eval(libcall(g:hassistant_library, "listFunctions", l:pandi))
    let b:hassistant_cache_hash = libcallnr(g:hassistant_library, "queryHash", l:pandi)
  endif
endfunction "}}}

function! hassistant#recache_functions() "{{{
  call hassistant#cache_functions()
  let l:pandi   = hassistant#mk_query()
  let l:curHash = libcallnr(g:hassistant_library, "queryHash", l:pandi)

  if b:hassistant_cache_hash != l:curHash
    let [b:hassistant_functions_cache, b:hassistant_types_cache] = eval(libcall(g:hassistant_library, "listFunctions", l:pandi))
    let b:hassistant_cache_hash = libcallnr(g:hassistant_library, "queryHash", l:pandi)
  endif
endfunction "}}}

function! hassistant#get_moduleName(str) "{{{
  if a:str =~# ' ".*" '
    return matchstr(a:str, '\<[A-Za-z0-9.]*\>', matchend(a:str, ' ".*" '))
  elseif a:str =~# '\C \<qualified\> '
    return matchstr(a:str, '\<[A-Za-z0-9.]*\>', matchend(a:str, '\C \<qualified\> '))
  elseif a:str =~# '\C^import '
    return matchstr(a:str, '\<[A-Za-z0-9.]*\>', matchend(a:str, '\C^import\> '))
  endif
  return 1
endfunction "}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
