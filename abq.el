;;; abq.el --- Emacs support for ABQ (Agent Bus Queue) -*- lexical-binding: t -*-

;; Author: Jason Walsh
;; URL: https://github.com/aygp-dr/abq
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;; Used by Makefile for org-mode tangling, linting, and export.
;; Can also be loaded interactively for ABQ development.

(require 'org)

;;; Configuration
(setq org-confirm-babel-evaluate nil)
(setq org-element-use-cache nil)

;;; Tangling

(defun abq/tangle-file (file)
  "Tangle FILE and report results."
  (message "Tangling %s..." file)
  (org-babel-tangle-file file))

(defun abq/detangle-file (file)
  "Detangle back into FILE from tangled sources."
  (message "Detangling %s..." file)
  (with-current-buffer (find-file-noselect file)
    (org-babel-detangle)))

;;; Export

(defun abq/export-to-pdf (file)
  "Export org FILE to PDF via LaTeX."
  (require 'ox-latex)
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (with-current-buffer (find-file-noselect file)
    (org-latex-export-to-pdf)))

(defun abq/export-to-html (file)
  "Export org FILE to HTML."
  (require 'ox-html)
  (with-current-buffer (find-file-noselect file)
    (org-html-export-to-html)))

(defun abq/export-to-md (file)
  "Export org FILE to Markdown."
  (require 'ox-md)
  (with-current-buffer (find-file-noselect file)
    (org-md-export-to-markdown)))

;;; Linting

(defun abq/lint-file (file)
  "Lint org FILE and print warnings."
  (with-current-buffer (find-file-noselect file)
    (let ((warnings (org-lint)))
      (when warnings
        (dolist (w warnings)
          (message "%s:%d: %s" file (car w) (cadr w)))))))

;;; Graphviz / Dot babel support
;;
;; Enables:
;;   - C-c C-c to evaluate dot blocks inline (ob-dot)
;;   - :comments link on dot blocks for bidirectional tangle/detangle
;;     (maps dot → c-mode which provides // comment syntax)
;;   - :file output for PNG/SVG generation

(require 'ob-dot)

(add-to-list 'org-babel-load-languages '(dot . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; Map dot to c-mode for comment syntax (needed by :comments link).
;; graphviz-dot-mode is not always available; c-mode provides // comments
;; which org-babel uses for link markers in tangled .dot files.
(add-to-list 'org-src-lang-modes '("dot" . c))

;; Define comment syntax for graphviz-dot-mode (needed for tangling with :comments)
(defun abq/setup-graphviz-comments ()
  "Set up comment syntax for graphviz-dot files."
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;; Apply to graphviz-dot-mode if available, otherwise create minimal mode
(with-eval-after-load 'graphviz-dot-mode
  (add-hook 'graphviz-dot-mode-hook #'abq/setup-graphviz-comments))

;; Fallback: define comment vars for dot tangling in batch mode
(add-to-list 'org-src-lang-modes '("dot" . c))  ; Use c-mode comments as fallback

(provide 'abq)
;;; abq.el ends here
