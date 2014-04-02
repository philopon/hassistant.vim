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

function! hassistant#file_hash() "{{{
  return libcallnr(g:hassistant_executable_directory . "library.so", "hashFile", expand('%'))
endfunction "}}}

function! hassistant#get_xflags_and_imports() "{{{
  let buf  = ""
  let lnum = 1
  let eob  = line('$')
  while 1
    let line = getline(lnum)
    if line =~# '{-#\s*LANGUAGE.*#-}'
      let buf = buf . "\n" . line
    endif
    if line =~# '^import ' || lnum > eob
      break
    endif
    let lnum += 1
  endwhile

  while 1
    let line = getline(lnum)
    let buf = buf . "\n" . line
    if line !~# '^\(import \| \|$\|#\)' || lnum > eob
      break
    endif
    let lnum += 1
  endwhile
  return buf
endfunction "}}}

function! hassistant#string_hash(str) "{{{
  return libcallnr(g:hassistant_executable_directory . "library.so", "hash", a:str)
endfunction "}}}

function! hassistant#buffer_hash() "{{{
  let imp = hassistant#get_xflags_and_imports()
  return hassistant#string_hash(imp)
endfunction "}}}

function! hassistant#start_type_process() "{{{
  if exists('b:hassistant_process')
    return 1
  endif
  let hash = hassistant#buffer_hash()
  if exists('b:hassistant_hash') && b:hassistant_hash == hash
    return 2
  endif
  let b:hassistant_tmp     = ''
  let b:hassistant_hash    = hash
  let b:hassistant_process = vimproc#popen2(g:hassistant_executable_directory . "types " . expand('%'))
  let imp = hassistant#get_xflags_and_imports()
  call b:hassistant_process.stdin.write(imp)
  call b:hassistant_process.stdin.close()
  return 0
endfunction "}}}

function! hassistant#finish_type_process() "{{{
  if !exists('b:hassistant_process')
    return 1
  endif
  call  b:hassistant_process.stdout.close()
  call  b:hassistant_process.waitpid()
  unlet b:hassistant_process
  return 0
endfunction "}}}

function! hassistant#check_type_process() "{{{
  if !exists('b:hassistant_process')
    return 1
  endif
  let b:hassistant_tmp = b:hassistant_tmp . b:hassistant_process.stdout.read()
  if b:hassistant_process.stdout.eof
    call hassistant#finish_type_process()
    let [b:hassistant_dict, b:hassistant_vars, b:hassistant_types] = eval(split(b:hassistant_tmp, "\n")[0])
    unlet b:hassistant_tmp
    return 0
  endif
  return -1
endfunction "}}}

function! hassistant#get_type(word) "{{{
  if exists('b:hassistant_dict')
    let unk = get(b:hassistant_dict, a:word, 1)
    if !unk
      let max = &columns * &cmdheight - 12
      let typ = a:word . ' :: ' . unk
      if len(typ) > max
        return typ[0:(max-5)] . ' ...'
      else
        return typ
      endif
    endif
  endif
  return ""
endfunction "}}}

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
