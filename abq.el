;;; abq.el --- Emacs support for ABQ (Agent Bus Queue) -*- lexical-binding: t; -*-

;; Author: JW
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;;; Commentary:

;; Emacs Lisp support for ABQ org files with Graphviz dot src blocks.
;; Provides:
;; - org-babel setup for dot/graphviz execution
;; - Font-lock keywords for ABQ message types in org src blocks
;; - Interactive rendering of the state machine diagram
;; - Batch export of dot blocks to SVG/PNG

;;; Code:

(require 'org)
(require 'ob-dot nil t)

;;; Babel setup

(defun abq-org-babel-setup ()
  "Register dot/graphviz as org-babel languages for ABQ."
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((dot . t)))))

;;; Dot export

(defvar abq-dot-default-format "png"
  "Default output format for dot rendering (png, svg, pdf).")

(defvar abq-dot-program "dot"
  "Path to the Graphviz dot executable.")

(defun abq-dot-export (dot-file &optional format output-file)
  "Export DOT-FILE to FORMAT (default `abq-dot-default-format').
Output written to OUTPUT-FILE or derived from DOT-FILE."
  (interactive
   (list (read-file-name "Dot file: " nil nil t nil
                         (lambda (f) (string-suffix-p ".dot" f)))))
  (let* ((fmt (or format abq-dot-default-format))
         (out (or output-file
                  (concat (file-name-sans-extension dot-file) "." fmt)))
         (cmd (format "%s -T%s -o %s %s"
                      (shell-quote-argument abq-dot-program)
                      (shell-quote-argument fmt)
                      (shell-quote-argument (expand-file-name out))
                      (shell-quote-argument (expand-file-name dot-file)))))
    (message "Rendering %s -> %s" dot-file out)
    (shell-command cmd)
    (message "Wrote %s" out)
    out))

;;; Interactive rendering

(defun abq-render-state-machine ()
  "Render the ABQ state machine diagram from the project dot file.
Looks for docs/abq-state-machine.dot relative to the project root."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory "abq-spec.org")
                   default-directory))
         (dot-file (expand-file-name "docs/abq-state-machine.dot" root)))
    (if (file-exists-p dot-file)
        (let ((out (abq-dot-export dot-file)))
          (when (and (display-graphic-p)
                     (file-exists-p out))
            (find-file out)))
      (user-error "State machine dot file not found: %s" dot-file))))

;;; Font-lock for ABQ types in org buffers

(defvar abq-message-types
  '("request" "response" "broadcast" "heartbeat" "shutdown" "ping" "exec")
  "ABQ message type keywords for font-lock highlighting.")

(defvar abq-directory-names
  '("requests" "responses" "processing" "archive" "channels" "agents")
  "ABQ directory names for font-lock highlighting.")

(defface abq-message-type-face
  '((t :foreground "#007bff" :weight bold))
  "Face for ABQ message type keywords.")

(defface abq-directory-face
  '((t :foreground "#28a745" :weight bold))
  "Face for ABQ directory names.")

(defvar abq-font-lock-keywords
  `((,(regexp-opt abq-message-types 'words) . 'abq-message-type-face)
    (,(regexp-opt abq-directory-names 'words) . 'abq-directory-face))
  "Font-lock keywords for ABQ terms in org src blocks.")

(defun abq-add-font-lock ()
  "Add ABQ font-lock keywords to the current buffer."
  (font-lock-add-keywords nil abq-font-lock-keywords))

;;; Tangle and render

(defun abq-tangle-and-render ()
  "Tangle ABQ spec then render dot files."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory "abq-spec.org")
                   default-directory))
         (spec (expand-file-name "abq-spec.org" root)))
    (when (file-exists-p spec)
      (org-babel-tangle-file spec)
      (abq-render-state-machine))))

;;; Minor mode

(define-minor-mode abq-mode
  "Minor mode for ABQ org file support.
Adds font-lock highlighting for ABQ message types and directory names."
  :lighter " ABQ"
  (if abq-mode
      (abq-add-font-lock)
    (font-lock-remove-keywords nil abq-font-lock-keywords))
  (font-lock-flush))

;;; Auto-activate for ABQ files

(defun abq-maybe-enable ()
  "Enable `abq-mode' if the current file looks like an ABQ org file."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (string-match-p "abq" (file-name-nondirectory (buffer-file-name))))
    (abq-mode 1)))

(add-hook 'org-mode-hook #'abq-maybe-enable)

(provide 'abq)
;;; abq.el ends here
