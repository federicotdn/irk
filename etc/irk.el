;;; irk.el  -*- lexical-binding: t -*-

(require 'xref)

(defconst irk--languages
  '((haskell-mode . "haskell")
    (python-mode . "python")
    (python-ts-mode . "python")
    (go-mode . "go")
    (go-ts-mode . "go")
    (c-mode . "c")
    (c-ts-mode . "c"))
  "Alist mapping major modes to irk language identifiers.")

(defgroup irk nil
  "A tool for finding symbols in codebases."
  :prefix "irk-"
  :group 'tools)

(defcustom irk-languages nil
  "List of languages to enable irk xref backend for.
Valid values are: \"haskell\", \"python\", \"go\", \"c\"."
  :type `(repeat (choice ,@(mapcar (lambda (lang) `(const ,lang))
                                   (delete-dups (mapcar #'cdr irk--languages)))))
  :group 'irk)

(defun irk-enable (&optional disable)
  "Enable irk xref backend globally.
With optional DISABLE argument, remove the hook instead."
  (if disable
      (remove-hook 'xref-backend-functions #'irk--xref-backend)
    (add-hook 'xref-backend-functions #'irk--xref-backend)))

(defun irk--eglot-managed-mode-hook ()
  "Add irk xref backend to buffer-local xref-backend-functions."
  ;; Eglot uses depth nil (i.e. 0), so 10 should have less priority.
  (add-hook 'xref-backend-functions #'irk--xref-backend 10 t))

(defun irk-eglot-enable (&optional disable)
  "Enable irk xref backend for eglot-managed buffers.
Adds irk--xref-backend to the buffer-local xref-backend-functions,
appending it at the end so eglot's backend is tried first.
With optional DISABLE argument, remove the hook instead."
  (require 'eglot)
  (if disable
      (remove-hook 'eglot-managed-mode-hook #'irk--eglot-managed-mode-hook)
    (add-hook 'eglot-managed-mode-hook #'irk--eglot-managed-mode-hook)))

(defun irk--xref-backend ()
  (let ((language (cdr (assoc major-mode irk--languages))))
    (when (and language (member language irk-languages))
      'irk)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'irk)))
  (thing-at-point 'symbol t))

(cl-defmethod xref-backend-definitions ((_backend (eql 'irk)) identifier)
  (let* ((root (expand-file-name (project-root (project-current t))))
         (language (cdr (assoc major-mode irk--languages)))
         (output (shell-command-to-string
                  (format "irk find -w %s -l %s %s"
                          (shell-quote-argument root)
                          (shell-quote-argument language)
                          (shell-quote-argument identifier)))))
    (mapcar
     (lambda (line)
       (when (string-match "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)" line)
         (xref-make identifier
                    (xref-make-file-location (match-string 1 line)
                                             (string-to-number (match-string 2 line))
                                             (string-to-number (match-string 3 line))))))
     (split-string output "\n" t))))
