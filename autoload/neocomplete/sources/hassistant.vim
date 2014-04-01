if exists('g:loaded_neocomplete_source_hassistant')
  finish
endif
let g:loaded_neocomplete_source_hassistant = 1

let s:save_cpo = &cpo
set cpo&vim

let s:source = {
  \ 'name': 'hassistant',
  \ 'kind': 'manual',
  \ 'rank': 200,
  \ 'filetypes': { 'haskell': 1 },
  \ 'min_pattern_length': 1,
  \ 'hooks': {},
  \ }

function! s:source.get_complete_position(context) "{{{
  call hassistant#check_type_process()
  let ret = eval(libcall(g:hassistant_executable_directory . "library.so", "position", a:context.input))
  let a:context.ret = ret
  if ret.mode == 0
    return neocomplete#helper#match_word(a:context.input)[0]
  endif
  return ret.position
endfunction "}}}

function! s:source.gather_candidates(context) "{{{
  if a:context.ret.mode == 1
    return eval(libcall(g:hassistant_executable_directory . "library.so", "gatherLANGUAGE", 0))
  elseif a:context.ret.mode == 2
    return eval(libcall(g:hassistant_executable_directory . "library.so", "gatherNamesInModule", expand('%') . "\n" . a:context.ret.module))
  elseif a:context.ret.mode == 3
    return  eval(libcall(g:hassistant_executable_directory . "library.so", "gatherNamesInConstructor", expand('%') . "\n" . a:context.ret.module . "\n" . a:context.ret.constructor))
  elseif a:context.ret.mode == 4
    return eval(libcall(g:hassistant_executable_directory . "library.so", "gatherModule", expand('%')))
  elseif a:context.ret.mode == 5
    return eval(libcall(g:hassistant_executable_directory . "library.so", "gatherTopLevel", 0))
  else
    if exists('b:hassistant_types')
      return deepcopy(b:hassistant_types)
    endif
    return []
  endif
endfunction "}}}

function! neocomplete#sources#hassistant#define() "{{{
  return s:source
endfunction "}}}

let &cpo = s:save_cpo
unlet s:save_cpo
