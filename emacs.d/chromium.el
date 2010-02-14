(defgroup chromium nil
  "Emacs settings for developing Chromium."
  :group 'programming)
(defcustom chromium-build-command "make -r -j3 chrome"
  "Initial build command used by `compile'."
  :type 'string
  :group 'chromium)

(defun upward-find-file (filename &optional dir)
  "Look for filename in the current directory or its parents;
return the directory or nil."
  (let ((dir (expand-file-name (if dir dir "."))))
    (cond ((file-exists-p (expand-file-name filename dir)) dir)
          ((string= dir "/") nil)
          (t (let ((parent (directory-file-name (file-name-directory dir))))
               (upward-find-file filename parent))))))

(defun chromium-setup-compile ()
  "Set up `compile' to default to Chromium's make command."
  (set (make-local-variable 'compile-command)
       (concat "cd " chromium-root "; "
               chromium-build-command)))

(defun chromium-setup-style ()
  ;; Get this module from http://code.google.com/p/google-styleguide/ .
  (require 'google-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style))

(defun chromium-maybe-setup ()
  "Test if we're in the Chrome tree and set up environment for
hacking on Chromium if so.
Meant to be added to `find-file-hook'."

  ;; We find the source root by looking for a crazy filename.
  (let ((root (upward-find-file "chrome/chrome.gyp")))
    (when root
      (defvar chromium-root root
        "*Path to root of containing Chromium tree.")
      (make-variable-buffer-local 'chromium-root)

      ;; (message "In Chromium dir %s; setting variables." chromium-root)
      (chromium-setup-compile)
      (chromium-setup-style))))

(add-hook 'find-file-hook 'chromium-maybe-setup)

;; Use python-mode for gyp files.
(add-to-list 'auto-mode-alist '("\\.gypi?$" . python-mode))

(provide 'chromium)
