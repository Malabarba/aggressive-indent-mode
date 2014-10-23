aggressive-indent-mode
======================

`electric-indent-mode` is enough to keep your code nicely aligned when
all you do is type. However, once you start shifting blocks around,
transposing lines, or slurping and barfing sexps, indentation is bound
to go wrong.

**`aggressive-indent-mode`** is a minor mode that keeps your code **always**
indented. It reindents after every command, making it more reliable
than `electric-indent-mode`.

### Demonstration ###

![aggressive-indent](aggressive-indent.gif)

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


*Plase Note:* `aggressive-indent` works superbly with lisp-like modes
 (where `end-of-defun`) works very nicely. However, in some c-like
 modes aggressive-indent
 [might cause some lag](https://github.com/Bruce-Connor/aggressive-indent-mode/issues/7),
 I'm working on that.

### Manual Installation ###

If you don't want to install from Melpa, you can download it manually,
place it in your `load-path` and require it with

    (require 'aggressive-indent)
