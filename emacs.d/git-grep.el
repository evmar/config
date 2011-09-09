;;; git-grep.el --- grep using git

;; Copyright 2011 - Evan Martin

;; Author: Evan Martin <martine@danga.com>
;; URL: https://github.com/martine/config/blob/master/emacs.d/git-grep.el
;; Version: 1.0

(defun chomp (str)
  (substring str 0 (- (length str) 1)))

(defun git-root ()
  (interactive)
  (chomp (shell-command-to-string "pwd; cd `git rev-parse --show-toplevel`; pwd")))

(defun git-grep ()
  (interactive)
  (let ((default-directory (chomp (shell-command-to-string "cd `git rev-parse --show-toplevel`; pwd"))))
    (message default-directory)
    (call-interactively 'rgrep)))

(global-set-key [f3] 'git-grep)

(provide 'git-grep)

;;; git-grep.el ends here