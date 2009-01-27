;; Get this module from http://code.google.com/p/google-styleguide/ .
(require 'google-c-style)

(defun upward-find-file (filename &optional dir)
  "Look for filename in the current directory or its parents;
return the directory or nil."
  (let ((dir (expand-file-name (if dir dir "."))))
    (cond ((file-exists-p (expand-file-name filename dir)) dir)
          ((string= dir "/") nil)
          (t (let ((parent (directory-file-name (file-name-directory dir))))
           (upward-find-file filename parent))))))

(defun chromium-setup-compile ()
  "Set up compile to build chromium's scons appropriately.
Meant to be added to `find-file-hook'."
  (interactive)

  (add-hook 'c-mode-common-hook 'google-set-c-style)

  ;; We find the source root by looking for a crazy filename.
  (let ((chrome-root (upward-find-file "WEBKIT_MERGE_REVISION")))
    (when chrome-root
      (set (make-local-variable 'compile-command)
           (concat "cd " chrome-root "/chrome; "
                   "../third_party/scons/scons.py "
                   "--site-dir=../site_scons --implicit-cache ")))))

(provide 'chromium)
