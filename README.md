aggressive-indent-mode    [![Say Thank You](https://img.shields.io/gratipay/Malabarba.svg)](https://gratipay.com/Malabarba/)
======================

`electric-indent-mode` is enough to keep your code nicely aligned when
all you do is type. However, once you start shifting blocks around,
transposing lines, or slurping and barfing sexps, indentation is bound
to go wrong.

**`aggressive-indent-mode`** is a minor mode that keeps your code **always**
indented. It reindents after every command, making it more reliable
than `electric-indent-mode`.

### Demonstration ###

- An example of Lisp mode (Emacs Lisp):
![Lisp Code Example](lisp-example.gif)

- An example of non-Lisp mode (C):
![C Code Example](c-example.gif)

### Instructions ###

This package is available fom Melpa, you may install it by calling

    M-x package-install RET aggressive-indent

Then activate it with

    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'css-mode-hook #'aggressive-indent-mode)

You can use this hook on any mode you want, `aggressive-indent` is not
exclusive to emacs-lisp code. In fact, if you want to turn it on for
every programming mode, you can do something like:

    (global-aggressive-indent-mode 1)
    (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

### Manual Installation ###

If you don't want to install from Melpa, you can download it manually,
place it in your `load-path` along with its two dependencies:

- [Names](https://github.com/Bruce-Connor/names/)
- and `cl-lib` (which you should already have if your `emacs-version` is at least 24.3).

Then require it with:

    (require 'aggressive-indent)
