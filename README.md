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

![LANGUAGE pragma completion](img/LANGUAGE.png)
![module completion](img/module.png)
![functions in module completion](img/functions_in_module.png)
![functions in module completion](img/functions_in_module.png)
![functions completion](img/function.png)
![qualified functions completion](img/qualified_function.png)
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

