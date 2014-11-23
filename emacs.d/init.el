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
 css-indent-offset 2
 python-indent 4
 css-indent-offset 2

 require-final-newline t

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
(global-auto-revert-mode 1)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.local/share/emacs/site-lisp")

; disable scroll bars
(scroll-bar-mode -1)

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
 '(evil-shift-width 2)
 '(evil-want-C-i-jump nil)
 '(haskell-program-name "ghci")
 '(ido-enable-tramp-completion nil)
 '(js-indent-level 2)
 '(org-agenda-files (quote ("~/everything.org")))
 '(paragraph-separate "[ 	]*$\\|-[ ]")
 '(paragraph-start "\\|[ 	]*$\\\\|-[ ]")
 '(safe-local-variable-values (quote ((c-offsets-alist (innamespace . 0)))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward))
 '(vc-handled-backends nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:underline "red")))))

; evil mode
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(add-to-list 'evil-emacs-state-modes 'grep-mode 'erc-mode)

(require 'devhelp)

; interactive buffer switch and file load
(require 'ido)
(ido-mode t)
; allow ido to read large directories
(setq ido-max-directory-size 100000)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

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
(setq compilation-scroll-output 'first-error)
;; If the compilation has a zero exit code, the window disappears
;; after two seconds.
;; (setq compilation-finish-functions
;;       (lambda (buf str)
;;         (unless (string-match "exited abnormally" str)
;;           (delete-windows-on buf)
;;           (message "compile succeeded"))))

; Timestamp function.
(defun timestamp ()
  "Insert a time stamp into the buffer."
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M" (current-time))))

(defun new-post ()
  "Set up a post for lazyblog."
  (interactive)
  (insert "Timestamp: ") (timestamp) (insert "\n")
  (insert "Subject: \n")
  (insert "Summary: \n"))

; Markdown
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

; Javascript
(require 'js)
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

; SCons
(add-to-list 'auto-mode-alist '("\\.scons$" . python-mode))
; vi-like keybindings

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
  (if (or (string= (buffer-file-name) (expand-file-name "~/.emacs.d/init.el"))
          (string= (buffer-file-name) (expand-file-name "~/projects/config/emacs.d/init.el")))
      (byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook 'autocompile)

; Only use whitespace mode for overlong lines; its end-of-line whitespace
; detection is annoying.
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode 1)

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
(add-hook 'go-mode-hook (lambda ()
                          (set-variable 'tab-width 4 t)))
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(require 'coffee-mode)

(require 'ninja-mode)

(if (file-exists-p "~/.emacs.d/magit/50magit.el")
    (progn
      (add-to-list 'load-path "~/.emacs.d/magit")
      (require '50magit)))

(require 'protobuf-mode)

(require 'rust-mode)

;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")
(add-to-list 'auto-mode-alist '("\\.mm$" . c++-mode))

(load "local" t t)

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

; Better font on Windows.
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :font "Consolas-11"))

; It's ok to run M-x erase-buffer.
(put 'erase-buffer 'disabled nil)

; Trim initial/trailing whitespace from a string.
(defun trim (s)
  (replace-regexp-in-string "\n*$" "" s))

; Command+key to run the line under the cursor as a shell command.
(defun shell-line ()
  "execute region as shell command"
  (interactive)
  (let* ((buf "*shell command*")
         (bds (bounds-of-thing-at-point 'line))
         (line (trim (buffer-substring-no-properties (car bds) (cdr bds)))))
    (if (get-buffer buf) (kill-buffer buf))
    (shell-command line buf)))
(global-set-key (kbd "M-@") 'shell-line)

(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

; Don't highlight random words in shell buffers.
(set-variable 'shell-font-lock-keywords nil)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-prompt-read-only t)

(require 'paredit)

(defface paren-face
  '((((class color))
     (:foreground "dark gray")))
  "Face for parens in lisp"
  :group 'faces)

(defun add-match-indent ()
  (put 'match 'scheme-indent-function 1)
  (put 'match-let 'scheme-indent-function 0))

(add-hook 'scheme-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("(\\|)" . 'paren-face)))
            (add-match-indent)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("(\\|)" . 'paren-face)))))

(add-hook 'lpaca-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("(\\|)" . 'paren-face)))))

(if (file-exists-p "~/projects/src/rust/src/etc/emacs")
    (progn
      (add-to-list 'load-path "~/projects/src/rust/src/etc/emacs")
      (require 'rust-mode)))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(require 'yasnippet)
(yas-global-mode 1)
(add-hook 'go-mode-hook '(lambda () (yas-minor-mode)))

(require 'web-mode)
(setq web-mode-script-padding 0)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; popwin hides popup windows like compilation/help automatically.
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-p") popwin:keymap)

;; custom mode line, based on
;; http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(setq-default
 mode-line-format
 '(
   ;; line/col
   "%4l:%2c  "

   ;; dir + file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-directory-face)
   (:propertize "%b"
                face mode-line-filename-face)

   ;; mod state
   (:eval (when (buffer-modified-p) "*"))

   "  "

   (vc-mode vc-mode)

   ;; major mode
   "[%m]"))

(make-face 'mode-line-directory-face)
(set-face-attribute 'mode-line-directory-face nil
                    :foreground "gray20")

(make-face 'mode-line-filename-face)
(set-face-attribute 'mode-line-filename-face nil
                    :weight 'bold)

(require 'sql)
(require 'toml-mode)
