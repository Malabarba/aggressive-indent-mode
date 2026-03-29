;;; aggressive-indent-tests.el --- Tests for aggressive-indent  -*- lexical-binding: t; -*-

(require 'ert)
(require 'aggressive-indent)

;; Prevent native-comp trampoline compilation from interfering with tests.
;; `aggressive-indent--softly-indent-defun' uses `cl-letf' to rebind
;; `message', which triggers trampoline compilation that can error in
;; environments without a working native compiler (e.g. CI, batch mode
;; without Xcode CLI tools on macOS).
(when (boundp 'native-comp-enable-subr-trampolines)
  (setq native-comp-enable-subr-trampolines nil))

(defun aggressive-indent-test--ai-idle-timer-count ()
  "Count aggressive-indent idle timers in `timer-idle-list'."
  (let ((count 0))
    (dolist (timer timer-idle-list)
      (when (eq (timer--function timer) 'aggressive-indent--indent-if-changed)
        (cl-incf count)))
    count))

(ert-deftest aggressive-indent-test-timer-cancelled-by-process-changed-list ()
  "`--process-changed-list-and-indent' must cancel the idle timer.
This function is called both from the idle timer callback and directly
from `before-save-hook'.  If it doesn't cancel the timer, the
`before-save-hook' path leaks repeating timers."
  (with-temp-buffer
    (emacs-lisp-mode)
    (aggressive-indent-mode 1)
    ;; Simulate a tracked change and an active timer.
    (setq aggressive-indent--changed-list '((1 1)))
    (setq aggressive-indent--idle-timer
          (run-with-idle-timer aggressive-indent-sit-for-time t
                               #'aggressive-indent--indent-if-changed (current-buffer)))
    (should aggressive-indent--idle-timer)
    (ignore-errors (aggressive-indent--process-changed-list-and-indent))
    (should-not aggressive-indent--idle-timer)
    (aggressive-indent-mode -1)))

(ert-deftest aggressive-indent-test-no-timer-leak-on-save ()
  "Saving a buffer with pending changes must not leak an idle timer.

`before-save-hook' calls `--process-changed-list-and-indent' directly,
bypassing the timer callback.  Without the fix, this empties the change
list but leaves the repeating idle timer running forever."
  (let ((tmpfile (make-temp-file "aggressive-indent-test-" nil ".el")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmpfile)
          (emacs-lisp-mode)
          (aggressive-indent-mode 1)
          (insert "(defun foo ()\n:bar)")
          ;; Timer should exist after the edit
          (should aggressive-indent--idle-timer)
          ;; Save — triggers before-save-hook
          (let ((save-silently t))
            (save-buffer))
          ;; Timer must be cancelled
          (should-not aggressive-indent--idle-timer)
          (aggressive-indent-mode -1)
          (kill-buffer))
      (ignore-errors (delete-file tmpfile)))))

(ert-deftest aggressive-indent-test-no-timer-leak-multiple-buffers ()
  "Saving multiple buffers must not accumulate leaked timers."
  (let ((tmpfiles (cl-loop for i below 5
                           collect (make-temp-file "aggressive-indent-test-" nil ".el")))
        (initial-count (aggressive-indent-test--ai-idle-timer-count))
        (bufs nil))
    (unwind-protect
        (progn
          (dolist (tmpfile tmpfiles)
            (let ((buf (find-file-noselect tmpfile)))
              (push buf bufs)
              (with-current-buffer buf
                (emacs-lisp-mode)
                (aggressive-indent-mode 1)
                (insert (format "(defun test-%s () :ok)" (file-name-base tmpfile)))
                (let ((save-silently t))
                  (save-buffer)))))
          ;; Buffers are still open — leaked timers would show up here
          (should (= initial-count (aggressive-indent-test--ai-idle-timer-count))))
      (dolist (buf bufs)
        (when (buffer-live-p buf)
          (with-current-buffer buf (aggressive-indent-mode -1))
          (kill-buffer buf)))
      (dolist (tmpfile tmpfiles)
        (ignore-errors (delete-file tmpfile))))))

(provide 'aggressive-indent-tests)
;;; aggressive-indent-tests.el ends here
