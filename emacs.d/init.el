(setq
 ; don't show startup screen
 inhibit-startup-screen t
 ; no toolbar
 tool-bar-mode nil
 ; disable backup
 backup-inhibited t
 ; disable auto save
 auto-save-default nil
 
 indent-tabs-mode nil
 standard-indent 2
 c-basic-offset 2
 python-indent 1
)

; (global-set-key "\C-m" 'newline-and-indent)
; (global-set-key "\C-j" 'newline)
(global-set-key "\C-xc" 'compile)

(menu-bar-mode 0)

; Git support.
(add-to-list 'load-path "~/.emacs.d")
(require 'git-emacs)
(require 'magit)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

; Haskell ghci support.
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(setq completion-ignored-extensions
      (cons ".hi" completion-ignored-extensions))

;; this means hitting the compile button always saves the buffer
;; having to separately hit C-x C-s is a waste of time
(setq mode-compile-always-save-buffer-p t)
;; make the compile window stick at 12 lines tall
(setq compilation-window-height 12)
 
;; If the compilation has a zero exit code, the windows disappears
;; after two seconds.
(setq compilation-finish-function
      (lambda (buf str)
        (unless (string-match "exited abnormally" str)
          ;; No errors, make the compilation window go away in a few seconds.
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))))
 
