;;; aggressive-indent.el --- Minor mode to aggressively keep your code always indented

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/aggressive-indent-mode
;; Version: 0.3.1
;; Package-Requires: ((emacs "24.1") (names "0.5") (cl-lib "0.5"))
;; Keywords: indent lisp maint tools
;; Prefix: aggressive-indent
;; Separator: -

;;; Commentary:
;;
;; `electric-indent-mode' is enough to keep your code nicely aligned when
;; all you do is type. However, once you start shifting blocks around,
;; transposing lines, or slurping and barfing sexps, indentation is bound
;; to go wrong.
;;
;; `aggressive-indent-mode' is a minor mode that keeps your code always
;; indented. It reindents after every command, making it more reliable
;; than `electric-indent-mode'.
;;
;; ### Instructions ###
;;
;; This package is available fom Melpa, you may install it by calling
;;
;;     M-x package-install RET aggressive-indent
;;
;; Then activate it with
;;
;;     (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;     (add-hook 'css-mode-hook #'aggressive-indent-mode)
;;
;; You can use this hook on any mode you want, `aggressive-indent' is not
;; exclusive to emacs-lisp code. In fact, if you want to turn it on for
;; every programming mode, you can do something like:
;;
;;     (global-aggressive-indent-mode 1)
;;     (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;;
;; ### Manual Installation ###
;;
;; If you don't want to install from Melpa, you can download it manually,
;; place it in your `load-path' and require it with
;;
;;     (require 'aggressive-indent)

;;; Instructions:
;;
;; INSTALLATION
;;
;; This package is available fom Melpa, you may install it by calling
;; M-x package-install RET aggressive-indent.
;;
;; Then activate it with
;;     (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;
;; You can also use an equivalent hook for another mode,
;; `aggressive-indent' is not exclusive to emacs-lisp code.
;;
;; Alternatively, you can download it manually, place it in your
;; `load-path' and require it with
;;
;;     (require 'aggressive-indent)

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Change Log:
;; 0.3.1 - 2014/10/30 - Define new delete-backward bound to backspace.
;; 0.3   - 2014/10/23 - Implement a smarter engine for non-lisp modes.
;; 0.2   - 2014/10/20 - Reactivate `electric-indent-mode'.
;; 0.2   - 2014/10/19 - Add variable `aggressive-indent-dont-indent-if', so the user can prevent indentation.
;; 0.1   - 2014/10/15 - Release.
;;; Code:

(require 'cl-lib)
(require 'names)

(defmacro aggressive-indent--do-softly (&rest body)
  "Execute body unobstrusively.
This means:
 1. Do nothing in several situations, specified by
    `aggressive-indent-dont-indent-if' and
    `aggressive-indent--internal-dont-indent-if'.
 2. Silence all messages.
 3. Never throw errors.
Meant for use in functions which go in hooks."
  (declare (debug t))
  `(unless (or (run-hook-wrapped
                'aggressive-indent--internal-dont-indent-if
                #'eval)
               (aggressive-indent--run-user-hooks))
     (cl-letf (((symbol-function 'message) #'ignore))
       (ignore-errors ,@body))))

;;;###autoload
(define-namespace aggressive-indent- :group indent

(defconst version "0.3.1" "Version of the aggressive-indent.el package.")
(defun bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and aggressive-indent versions."
  (interactive)
  (message "Your `aggressive-indent-version' is: %s, and your emacs version is: %s.
Please include this in your report!"
           version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/aggressive-indent-mode/issues/new"))


;;; Start of actual Code:
(defcustom dont-electric-modes '(ruby-mode)
  "List of major-modes where `electric-indent-mode' should be disabled."
  :type '(choice
          (const :tag "Never use `electric-indent-mode'." t)
          (repeat :tag "List of major-modes to avoid `electric-indent-mode'." symbol))
  :package-version '(aggressive-indent . "0.3.1"))

(defcustom excluded-modes
  '(
    bibtex-mode
    coffee-mode
    erc-mode
    jabber-chat-mode
    haml-mode
    minibuffer-inactive-mode
    python-mode
    special-mode
    tabulated-list-mode
    text-mode
    yaml-mode
    )
  "Modes in which `aggressive-indent-mode' should not be activated.
This variable is only used if `global-aggressive-indent-mode' is
active. If the minor mode is turned on with the local command,
`aggressive-indent-mode', this variable is ignored."
  :type '(repeat symbol)
  :package-version '(aggressive-indent . "0.3.1"))

(defcustom protected-commands '(undo undo-tree-undo undo-tree-redo)
  "Commands after which indentation will NOT be performed.
Aggressive indentation could break things like `undo' by locking
the user in a loop, so this variable is used to control which
commands will NOT be followed by a re-indent."
  :type '(repeat symbol)
  :package-version '(aggressive-indent . "0.1"))

(defcustom comments-too nil
  "If non-nil, aggressively indent in comments as well."
  :type 'boolean
  :package-version '(aggressive-indent . "0.3"))

(defvar -internal-dont-indent-if
  '((memq this-command aggressive-indent-protected-commands)
    (region-active-p)
    buffer-read-only
    (null (buffer-modified-p))
    (string-match "\\`[[:blank:]]*\n?\\'" (thing-at-point 'line))
    (and (not aggressive-indent-comments-too)
         (aggressive-indent--in-comment-p))
    (aggressive-indent--in-string-p))
  "List of forms which prevent indentation when they evaluate to non-nil.
This is for internal use only. For user customization, use
`aggressive-indent-dont-indent-if' instead.")

(defcustom modes-to-prefer-defun '(emacs-lisp-mode lisp-mode scheme-mode)
  "List of major-modes in which indenting defun is preferred.
Add here any major modes with very good definitions of
`end-of-defun' and `beginning-of-defun', or modes which bug out
if you have `after-change-functions' (such as paredit).

If current major mode is derived from one of these,
`aggressive-indent-mode' will call
`aggressive-indent-indent-defun' after every command. Otherwise,
it will call `aggressive-indent-indent-region-and-on' after every
buffer change."
  :type '(repeat symbol)
  :package-version '(aggressive-indent . "0.3"))

(eval-after-load 'yasnippet
  '(when (boundp 'yas--active-field-overlay)
     (add-to-list 'aggressive-indent--internal-dont-indent-if
                  '(and
                    (overlayp yas--active-field-overlay)
                    (overlay-end yas--active-field-overlay))
                  'append)))
(eval-after-load 'company
  '(when (boundp 'company-candidates)
     (add-to-list 'aggressive-indent--internal-dont-indent-if
                  'company-candidates)))
(eval-after-load 'auto-complete
  '(when (boundp 'ac-completing)
     (add-to-list 'aggressive-indent--internal-dont-indent-if
                  'ac-completing)))

(defcustom dont-indent-if '()
  "List of variables and functions to prevent aggressive indenting.
This variable is a list where each element is a lisp form.
As long as any one of these forms returns non-nil,
aggressive-indent will not perform any indentation.

See `aggressive-indent--internal-dont-indent-if' for usage examples."
  :type '(repeat sexp)
  :group 'aggressive-indent
  :package-version '(aggressive-indent . "0.2"))

(defvar -error-message
  "One of the forms in `aggressive-indent-dont-indent-if' had the following error, I've disabled it until you fix it: %S"
  "Error message thrown by `aggressive-indent-dont-indent-if'.")

(defvar -has-errored nil
  "Keep track of whether `aggressive-indent-dont-indent-if' is throwing.
This is used to prevent an infinite error loop on the user.")

(defun -run-user-hooks ()
  "Safely run forms in `aggressive-indent-dont-indent-if'.
If any of them errors out, we only report it once until it stops
erroring again."
  (and dont-indent-if
       (condition-case er
           (prog1 (eval (cons 'or dont-indent-if))
             (setq -has-errored nil))
         (error
          (unless -has-errored
            (setq -has-errored t)
            (message -error-message er))))))

:autoload
(defun indent-defun ()
  "Indent current defun.
Throw an error if parentheses are unbalanced."
  (interactive)
  (let ((p (point-marker)))
    (set-marker-insertion-type p t)
    (indent-region
     (save-excursion (beginning-of-defun 1) (point))
     (save-excursion (end-of-defun 1) (point)))
    (goto-char p)))

(defun -softly-indent-defun ()
  "Indent current defun unobstrusively.
Like `aggressive-indent-indent-defun', but wrapped in a
`aggressive-indent--do-softly'."
  (unless (or (run-hook-wrapped
               'aggressive-indent--internal-dont-indent-if
               #'eval)
              (aggressive-indent--run-user-hooks))
    (cl-letf (((symbol-function 'message) #'ignore))
      (ignore-errors (indent-defun)))))

:autoload
(defun indent-region-and-on (l r)
  "Indent region between L and R, and then some.
Call `indent-region' between L and R, and then keep indenting
until nothing more happens."
  (interactive "r")
  (let ((p (point-marker))
        was-begining-of-line)
    (set-marker-insertion-type p t)
    (unwind-protect
        (progn
          (goto-char r)
          (setq was-begining-of-line
                (= r (line-beginning-position)))
          ;; If L is at the end of a line, skip that line.
          (unless (= l r)
            (goto-char l)
            (when (= l (line-end-position))
              (cl-incf l)))
          ;; Indent the affected region.
          (unless (= l r) (indent-region l r))
          ;; `indent-region' doesn't do anything if R was the beginning of a line, so we indent manually there.
          (when was-begining-of-line
            (indent-according-to-mode))
          ;; And then we indent each following line until nothing happens.
          (forward-line 1)
          (while (and (null (eobp))
                      (/= (progn (skip-chars-forward "[:blank:]\n")
                                 (point))
                          (progn (indent-according-to-mode)
                                 (point))))
            (forward-line 1)))
      (goto-char p))))

(defun -softly-indent-region-and-on (l r &rest _)
  "Indent current defun unobstrusively.
Like `aggressive-indent-indent-region-and-on', but wrapped in a
`aggressive-indent--do-softly'."
  (unless (or (run-hook-wrapped
               'aggressive-indent--internal-dont-indent-if
               #'eval)
              (aggressive-indent--run-user-hooks))
    (cl-letf (((symbol-function 'message) #'ignore))
      (ignore-errors (indent-region-and-on l r)))))

(defvar -changed-list-right nil
  "List of right limit of regions changed in the last command loop.")

(defvar -changed-list-left nil
  "List of left limit of regions changed in the last command loop.")

(defun -indent-if-changed ()
  "Indent any region that changed in the last command loop."
  (let ((inhibit-modification-hooks t))
    (when -changed-list-left
      (-softly-indent-region-and-on
       (apply #'min -changed-list-left)
       (apply #'max -changed-list-right))
      (setq -changed-list-left nil
            -changed-list-right nil))))

(defun -keep-track-of-changes (l r &rest _)
  "Store the limits of each change that happens in the buffer."
  (push l -changed-list-left)
  (push r -changed-list-right))

(defun -in-comment-p ()
  "Return non-nil if point is inside a comment.
Assumes that the syntax table is sufficient to find comments."
  (nth 4 (syntax-ppss)))

(defun -in-string-p ()
  "Return non-nil if point is inside a string.
Assumes that the syntax table is sufficient for recognizing
strings."
  (nth 3 (syntax-ppss)))


;;; Minor modes
:autoload
(define-minor-mode mode nil nil " =>"
  '(("" . aggressive-indent-indent-defun)
    ([backspace] menu-item "maybe-delete-indentation" ignore
     :filter (lambda (&optional _)
               (when (and (looking-back "^[[:blank:]]+")
			  ;; Wherever we don't want to indent, we probably also
			  ;; want the default backspace behavior.
			  (not (run-hook-wrapped
				'aggressive-indent--internal-dont-indent-if
				#'eval))
			  (not (aggressive-indent--run-user-hooks)))
		 #'delete-indentation))))
  (if mode
      (if (and global-aggressive-indent-mode
               (or (cl-member-if #'derived-mode-p excluded-modes)
                   buffer-read-only))
          (mode -1)
        ;; Should electric indent be ON or OFF?
        (if (or (eq dont-electric-modes t)
                (cl-member-if #'derived-mode-p dont-electric-modes))
            (-local-electric nil)
          (-local-electric t))
        (if (cl-member-if #'derived-mode-p modes-to-prefer-defun)
            (add-hook 'post-command-hook #'-softly-indent-defun nil 'local)
          (add-hook 'after-change-functions #'-keep-track-of-changes nil 'local)
          (add-hook 'post-command-hook #'-indent-if-changed nil 'local)))
    ;; Clean the hooks
    (remove-hook 'after-change-functions #'-keep-track-of-changes 'local)
    (remove-hook 'post-command-hook #'-indent-if-changed 'local)
    (remove-hook 'post-command-hook #'-softly-indent-defun 'local)))

(defun -local-electric (on)
  "Turn `electric-indent-mode' on or off locally, as given by boolean ON."
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode (if on 1 -1))
    (set (make-local-variable 'electric-indent-mode) on)))

:autoload
(define-globalized-minor-mode global-aggressive-indent-mode
  mode mode)

:autoload
(defalias 'aggressive-indent-global-mode
  #'global-aggressive-indent-mode)
)

(provide 'aggressive-indent)
;;; aggressive-indent.el ends here.
