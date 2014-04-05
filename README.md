hassistant.vim
==============

Haskell completion function for vim using neocomplete.

features
==============
* using shared library
* robust completion
* display type

require
==============
* Shougo/vimproc.vim
* Shougo/neocomplete.vim

installation
==============
```.vim
NeoBundle 'philopon/hassistant.vim', 
      \ { 'build'  : { 'mac' : 'sh build.sh'
      \              , 'unix': 'sh build.sh' 
      \              }
      \ , 'depends': 'Shougo/vimproc' 
      \ }
```

screenshots
==============

completion
-----------

### LANGUAGE
![LANGUAGE](img/LANGUAGE.png)

### module
module name

![module name](img/module.png)

names in module

![names in module](img/namesInModule.png)

names in constructor

![names in constructor](img/namesInConstructor.png)

### Names
unqualified

![unqualified](img/unqualified.png)

qualified as

![qualified as](img/qualifiedAs.png)

qualified

![qualified](img/qualified.png)

type display
--------------
![type display](img/typeDisplay.png)
