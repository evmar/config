(require 'thingatpt)

(defun devhelp-strip (string)
  "Return string with trailing newline stripped."
  (replace-regexp-in-string "\n$" "" string))

(defun devhelp-current-symbol ()
  "Return the symbol at the point."
  ; thing-at-point includes properties, lame.
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun devhelp-read-query ()
  "Read a search query from the minibuffer."
  (let ((default (devhelp-current-symbol)))
    (read-string (if default
                     (format "Devhelp query (%s): " default)
                   "Devhelp query: ")
                 nil nil default)))

(defun devhelp (query)
  "Query devhelp for a keyword, opening the result in a browser."
  (interactive (list (devhelp-read-query)))
  (unless (string= "" query)
    (let ((path (devhelp-strip
                 (shell-command-to-string (concat "devhelp-query " query)))))
      (if (string= "" path)
          (message (format "devhelp: no match for '%s'" query))
        (browse-url (concat "file://" path))))))

(provide 'devhelp)
