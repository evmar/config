;;; vc-git.el --- VC backend for the git version control system

;; Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: Alexandre Julliard <julliard@winehq.org>
;; Keywords: tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains a VC backend for the git version control
;; system.
;;

;;; Installation:

;; To install: put this file on the load-path and add Git to the list
;; of supported backends in `vc-handled-backends'; the following line,
;; placed in your ~/.emacs, will accomplish this:
;;
;;     (add-to-list 'vc-handled-backends 'Git)

;;; Todo:
;;  - check if more functions could use vc-git-command instead
;;     of start-process.
;;  - changelog generation

;; Implement the rest of the vc interface. See the comment at the
;; beginning of vc.el. The current status is:
;; ("??" means: "figure out what to do about it")
;;
;; FUNCTION NAME                                   STATUS
;; BACKEND PROPERTIES
;; * revision-granularity                          OK
;; STATE-QUERYING FUNCTIONS
;; * registered (file)                             OK
;; * state (file)                                  OK
;; - state-heuristic (file)                        NOT NEEDED
;; * working-revision (file)                       OK
;; - latest-on-branch-p (file)                     NOT NEEDED
;; * checkout-model (files)                        OK
;; - workfile-unchanged-p (file)                   OK
;; - mode-line-string (file)                       OK
;; STATE-CHANGING FUNCTIONS
;; * create-repo ()                                OK
;; * register (files &optional rev comment)        OK
;; - init-revision (file)                          NOT NEEDED
;; - responsible-p (file)                          OK
;; - could-register (file)                         NOT NEEDED, DEFAULT IS GOOD
;; - receive-file (file rev)                       NOT NEEDED
;; - unregister (file)                             OK
;; * checkin (files rev comment)                   OK
;; * find-revision (file rev buffer)               OK
;; * checkout (file &optional editable rev)        OK
;; * revert (file &optional contents-done)         OK
;; - rollback (files)                              COULD BE SUPPORTED
;; - merge (file rev1 rev2)                   It would be possible to merge
;;                                          changes into a single file, but when
;;                                          committing they wouldn't
;;                                          be identified as a merge
;;                                          by git, so it's probably
;;                                          not a good idea.
;; - merge-news (file)                     see `merge'
;; - steal-lock (file &optional revision)          NOT NEEDED
;; HISTORY FUNCTIONS
;; * print-log (files &optional buffer shortlog)   OK
;; - log-view-mode ()                              OK
;; - show-log-entry (revision)                     OK
;; - comment-history (file)                        ??
;; - update-changelog (files)                      COULD BE SUPPORTED
;; * diff (file &optional rev1 rev2 buffer)        OK
;; - revision-completion-table (files)             OK
;; - annotate-command (file buf &optional rev)     OK
;; - annotate-time ()                              OK
;; - annotate-current-time ()                      NOT NEEDED
;; - annotate-extract-revision-at-line ()          OK
;; TAG SYSTEM
;; - create-tag (dir name branchp)                 OK
;; - retrieve-tag (dir name update)                OK
;; MISCELLANEOUS
;; - make-version-backups-p (file)                 NOT NEEDED
;; - repository-hostname (dirname)                 NOT NEEDED
;; - previous-revision (file rev)                  OK
;; - next-revision (file rev)                      OK
;; - check-headers ()                              COULD BE SUPPORTED
;; - clear-headers ()                              NOT NEEDED
;; - delete-file (file)                            OK
;; - rename-file (old new)                         OK
;; - find-file-hook ()                             NOT NEEDED

(eval-when-compile
  (require 'grep))

;; Derived from `lgrep'.
(defun vc-git-grep (regexp &optional files dir)
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While git grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
				   nil nil 'grep-history)
	     nil))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "In directory: "
					  nil default-directory t)))
	   (list regexp files dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (null files)
	  (if (string= command "git grep")
	      (setq command nil))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(setq command
	      (grep-expand-template "git grep -n -e <R> -- <F>" regexp files))
	(when command
	  (if (equal current-prefix-arg '(4))
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-history))
	    (add-to-history 'grep-history command))))
      (when command
	(let ((default-directory dir)
	      (compilation-environment '("PAGER=")))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start command 'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))

(provide 'git-grep)
