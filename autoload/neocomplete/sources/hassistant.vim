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
  let a:context.source__ret = ret
  return ret.position
endfunction "}}}

function! s:source.gather_candidates(context) "{{{
  if a:context.source__ret.mode == g:hassistant_modes.LANGUAGE
    return eval(libcall(g:hassistant_executable_directory . "library.so", "gatherLANGUAGE", 0))
  elseif a:context.source__ret.mode == g:hassistant_modes.NamesInModule
    return eval(libcall(g:hassistant_executable_directory . "library.so", "gatherNamesInModule",
          \ expand('%') . "\n" . a:context.source__ret.module))
  elseif a:context.source__ret.mode == g:hassistant_modes.NamesInConstructor
    return  eval(libcall(g:hassistant_executable_directory . "library.so", "gatherNamesInConstructor", 
          \ expand('%') . "\n" . a:context.source__ret.module . "\n" . a:context.source__ret.constructor))
  elseif a:context.source__ret.mode == g:hassistant_modes.Module
    return eval(libcall(g:hassistant_executable_directory . "library.so", "gatherModule", expand('%')))
  elseif a:context.source__ret.mode == g:hassistant_modes.TopLevel
    return eval(libcall(g:hassistant_executable_directory . "library.so", "gatherTopLevel", 0))
  elseif  a:context.source__ret.mode == g:hassistant_modes.Other
    if a:context.source__ret.type
      return deepcopy( get(b:, 'hassistant_types', []) )
    else
      return deepcopy( get(b:, 'hassistant_types', []) + get(b:, 'hassistant_vars', []))
    endif
  else
    return []
  endif
endfunction "}}}

function! neocomplete#sources#hassistant#define() "{{{
  return s:source
endfunction "}}}

let &cpo = s:save_cpo
unlet s:save_cpo
