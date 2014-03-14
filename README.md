hassistant.vim
==============

Haskell completion function for vim using neocomplete.

features
==============
* using shared library
* robust completion
* display type

screenshots
==============

LANGUAGE Pragma
------------
![LANGUAGE pragma](img/LANGUAGE.png)

module name
------------
![module name](img/module.png)

function in module
--------------
![functions in module completion](img/functions_in_module.png)

function
--------------
![function](img/function.png)

qualified function
--------------
![qualified function](img/qualified_function.png)

type
--------------
![type completion](img/type.png)

installation
==============

NeoBundle
```.vim
NeoBundle 'philopon/hassistant.vim', { 'build' : {'mac' : 'sh build.sh'} }
```

TODO
==============
* read package by PackageImports
* cabal
* type check

