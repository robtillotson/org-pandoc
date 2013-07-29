;;; ox-pandoc.el --- Pandoc backend for org-mode exporter

;; Copyright 2013 Rob Tillotson

;; Author: Rob Tillotson <rob@pyrite.org>

;;; Commentary:

;; This is a pandoc exporter for Org, built upon Markdown as an
;; intermediate format.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-md)

;;; User Modifiable Variables:

(defgroup org-export-pandoc nil
  "Options specific to Pandoc export back-end."
  :tag "Org Pandoc"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-pandoc-process-after-export t
  "Run pandoc to process the file after exporting it?"
  :group 'org-export-pandoc
  :type '(choice
          (const :tag "Yes" t)
          (const :tag "No" nil)))

(defcustom org-pandoc-command "pandoc"
  "Command to run pandoc."
  :group 'org-export-pandoc
  :type 'string)

(defcustom org-pandoc-extra-options ""
  "Extra pandoc options to use every time.
For example, if you encounter stack overflows, put the options
to expand the stack here."
  :group 'org-export-pandoc
  :type 'string)

(defcustom org-pandoc-output-format 'html
  "Default output format for pandoc conversion."
  :group 'org-export-pandoc
  :type 'symbol)

(defcustom org-pandoc-output-standalone t
  "Should output be a single standalone file or not?"
  :group 'org-export-pandoc
  :type 'boolean)

(org-export-define-derived-backend 'pandoc 'md
  :menu-entry
  '(?d "Export to Pandoc"
       ((?P "Intermediate to temp buffer"
            (lambda (a s v b) (org-pandoc-export-as-pandoc a s v)))
        (?p "To file"
            (lambda (a s v b) (org-pandoc-export-to-pandoc a s v)))))
  :translate-alist '((template . org-pandoc-template)))

(defun org-pandoc-template (contents info)
  (let ((title (org-export-data (plist-get info :title) info))
        (author (org-export-data (plist-get info :author) info)))
    (concat "% " title "\n"
            "% " author "\n"
            "\n"
            contents)))

(defun org-pandoc-export-as-pandoc (&optional async subtreep visible-only)
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Pandoc Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (markdown-mode)
              (org-export-add-to-stack (current-buffer) 'pandoc)))
        `(org-export-as 'pandoc ,subtreep ,visible-only))
    (let ((outbuf (org-export-to-buffer 'pandoc "*Org Pandoc Export*" subtreep visible-only)))
      (with-current-buffer outbuf (markdown-mode))
      (when org-export-show-temporary-export-buffer
        (switch-to-buffer-other-window outbuf)))))

(defun org-pandoc-run-pandoc (filename outfilename format &optional options)
  (let* ((args (list "-t" (symbol-name format)
                     "-o" outfilename
                     org-pandoc-extra-options
                     options
                     filename))
         (command (concat org-pandoc-command " " (mapconcat 'identity args " "))))
    (message "Running pandoc: %s" (shell-command-to-string command))))

(defun org-pandoc-export-to-file (&optional outfile subtreep visible-only)
  (let* ((format org-pandoc-output-format)
         (standalone org-pandoc-output-standalone)
         (options (concat (if standalone " -s")))
         (pandoc-output (concat (file-name-base outfile) "." (symbol-name format))))
    (org-export-to-file 'pandoc outfile subtreep visible-only)
    (org-pandoc-run-pandoc outfile pandoc-output format options)
    ))

(defun org-pandoc-export-to-pandoc (&optional async subtreep visible-only)
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (if async
        (org-export-async-start
            (lambda (f) (org-export-add-to-stack f 'pandoc))
          `(expand-file-name
            (org-pandoc-export-to-file ,outfile ,subtreep ,visible-only)))
      (org-pandoc-export-to-file outfile subtreep visible-only))))

(provide 'ox-pandoc)
