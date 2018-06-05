guile-colorized
===============

Because Microsoft has aquired GitHub, as a congrats, I would like to migrate this project to GitLab and this repo will not be maintained on GitHub anymore: https://gitlab.com/NalaGinrut/guile-colorized
=========

colorized REPL for GNU Guile
NOTE: Now guile-colorized only support Guile-2.0.9 or higher!
Drop your Guile-1.8.x from now on ;-)

## INSTALL
Just type "sudo make install". (There's no need to run "make")

## TEST
Copy these lines below to your REPL for test:

```scheme
(use-modules (oop goops) (rnrs) (ice-9 colorized))
(activate-colorized)
`(this-is-a-symbol 1 2.5 2/5 #\c "asdf" ,(lambda () #t) ,(cons 1 2) ,
  (vector 1 2 3) #2u32@2@3((1 2) (3 4)) ,(make-bytevector 10 99) ,<object>)
```


## ENJOY
And you may add these to your ~/.guile
```scheme
(use-modules (ice-9 colorized))
(activate-colorized)
```

Enjoy it!
