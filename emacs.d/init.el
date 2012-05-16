(setq-default
 ; don't show startup screen
 inhibit-startup-screen t
 ; disable backup
 backup-inhibited t
 ; disable auto save
 auto-save-default nil

 make-backup-files nil

 ; don't do all this renaming/copying jiggery-pokery when saving a file.
 file-precious-flag nil
 ; don't break hard links when editing
 backup-by-copying-when-linked nil

 indent-tabs-mode nil
 standard-indent 2
 c-basic-offset 2
 python-indent 4

 show-trailing-whitespace t
 require-final-newline t  ; will this break stuff?

 ; middle-click should paste at the point, not where I clicked.
 mouse-yank-at-point t

 ; death to fsync (really to ext3 with mode=ordered)
 write-region-inhibit-fsync t

 ; use normal monospace font
 font-use-system-font t
)

; no menu bar on console mode
(menu-bar-mode -1)

; reduce big gray margins on window
(fringe-mode '(1 . 0))

; auto-revert to on-disk file versions
(global-auto-revert-mode)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.local/share/emacs/site-lisp")

;(add-to-list 'load-path "~/projects/devhelp-index")
;(require 'devhelp)

; start emacs server
(server-start)

; interactive buffer switch and file load
(require 'ido)
(ido-mode t)
; allow ido to read large directories
(setq ido-max-directory-size 100000)

; Don't require me to type out "yes".
(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-nick "evmar")
 '(erc-server "irc.oftc.net")
 '(erc-user-full-name "Evan Martin")
 '(haskell-program-name "ghci")
 '(js-indent-level 2)
 '(js2-auto-indent-flag nil)
 '(js2-electric-keys (quote nil))
 '(js2-mirror-mode nil)
 '(js2-mode-escape-quotes nil)
 '(org-agenda-files (quote ("~/everything.org")))
 '(paragraph-separate "[ 	]*$\\|-[ ]")
 '(paragraph-start "\\|[ 	]*$\\\\|-[ ]")
 '(safe-local-variable-values (quote ((js2-basic-offset . 2) (c-offsets-alist (innamespace . 0)))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward))
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Arch))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:underline t)))))

; Haskell ghci support.
;(require 'inf-haskell)
;(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq completion-ignored-extensions
      (cons ".hi" completion-ignored-extensions))
(add-to-list 'auto-mode-alist '("\\.cpphs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.chs$" . haskell-mode))

;; this means hitting the compile button always saves the buffer
;; having to separately hit C-x C-s is a waste of time
(setq mode-compile-always-save-buffer-p t)
;; make the compile window stick at 12 lines tall
(setq compilation-window-height 12)
;; always scroll
(setq compilation-scroll-output t)
;; If the compilation has a zero exit code, the windows disappears
;; after two seconds.
(setq compilation-finish-functions
      (lambda (buf str)
        (unless (string-match "exited abnormally" str)
          ;; No errors, make the compilation window go away in a few seconds.
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))))

; Timestamp function.
(defun timestamp ()
  "Insert a time stamp into the buffer."
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M" (current-time))))

(defun new-post ()
  "Set up a post for lazyblog."
  (interactive)
  (insert "Timestamp: ") (timestamp) (insert "\n")
  (insert "Subject: "))

; Markdown
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

; SCons
(add-to-list 'auto-mode-alist '("\\.scons$" . python-mode))
; vi-like keybindings

(defvar newline-and-indent nil
  "Modify the behavior of the open-*-line functions to cause them
to autoindent.")

(defun open-previous-line (arg)
  "Open a new line before the current one.

See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun open-next-line (arg)
  "Move to the next line and then opens a line.

See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

(global-set-key (kbd "M-`") 'next-error)

(global-set-key [f7] (lambda ()
                       (interactive)
                       (save-excursion (switch-to-buffer "*compilation*")
                                       (recompile))))
(global-set-key (kbd "M-`") 'next-error)

(defun autocompile nil
  "compile itself if ~/.emacs/init.el"
  (interactive)
  (require 'bytecomp)
  (print (buffer-file-name))
  (if (or (string= (buffer-file-name) (expand-file-name "~/.emacs.d/init.el"))
          (string= (buffer-file-name) (expand-file-name "~/projects/config/emacs.d/init.el")))
      (byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook 'autocompile)

; Only use whitespace mode for overlong lines; its end-of-line whitespace
; detection is annoying.
(require 'whitespace)
(setq whitespace-style '(lines-tail))
(global-whitespace-mode)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (getenv "BROWSER"))
;(require 'w3m-load)
;(setq browse-url-browser-function 'w3m-browse-url)

(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

(require 'git-grep)

(require 'go-mode-load)

(require 'coffee-mode)

(require 'ninja-mode)

(require 'js)
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(require '50magit)

;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")
(add-to-list 'auto-mode-alist '("\\.mm$" . c++-mode))

(defun ami-summarize-indentation-at-point ()
  "Echo a summary of how one gets from the left-most column to
  POINT in terms of indentation changes."
  (interactive)
  (save-excursion
    (let ((cur-indent most-positive-fixnum)
          (trace '()))
      (while (not (bobp))
        (let ((current-line (buffer-substring (line-beginning-position)
                                              (line-end-position))))
          (when (and (not (string-match "^\\s-*$" current-line))
                     (< (current-indentation) cur-indent))
            (setq cur-indent (current-indentation))
            (setq trace (cons current-line trace))
            (if (or (string-match "^\\s-*}" current-line)
                    (string-match "^\\s-*else " current-line)
                    (string-match "^\\s-*elif " current-line))
                (setq cur-indent (1+ cur-indent)))))
        (forward-line -1))
      (message "%s" (mapconcat 'identity trace "\n")))))
