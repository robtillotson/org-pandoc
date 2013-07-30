;;; ox-pandoc.el --- Org exporter using Pandoc

;; Copyright 2013 Rob Tillotson

;; Author: Rob Tillotson <rob@pyrite.org>
;; Created: 2013-07-29
;; Version: 1.0
;; Package-Requires: ((org "8.0"))

;; This file is not part of GNU Emacs.

;;; License:

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This is a pandoc exporter for Org, built upon Markdown as an
;; intermediate format, and targeted at production of e-books in ePub format.

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

(defcustom org-pandoc-output-format 'epub
  "Default output format for pandoc conversion."
  :group 'org-export-pandoc
  :type 'symbol)

(defcustom org-pandoc-output-standalone t
  "Should output be a single standalone file or not?"
  :group 'org-export-pandoc
  :type 'boolean)

(defcustom org-pandoc-epub-rights nil
  "Copyright/license statement to include in EPUB metadata."
  :group 'org-export-pandoc
  :type 'string)

(defcustom org-pandoc-epub-stylesheet nil
  "Stylesheet to apply to EPUB files."
  :group 'org-export-pandoc
  :type 'string)

(defvar org-pandoc---epub-metadata nil)
(defvar org-pandoc---epub-cover-filename nil)
(defvar org-pandoc---epub-stylesheet-filename nil)
(defvar org-pandoc---command-options nil)

(org-export-define-derived-backend 'pandoc 'md
  :menu-entry
  '(?p "Export with Pandoc"
       ((?P "Markdown to buffer"
            (lambda (a s v b) (org-pandoc-export-as-pandoc a s v)))
        (?p "To file"
            (lambda (a s v b) (org-pandoc-export-to-pandoc a s v)))))
  :translate-alist '((template . org-pandoc-template))
  :options-alist '((:epub-rights "EPUB_RIGHTS" nil org-pandoc-epub-rights t)
                   (:epub-cover "EPUB_COVER" nil nil t)
                   (:epub-stylesheet "EPUB_STYLESHEET" nil org-pandoc-epub-stylesheet t)
                   (:pandoc-options "PANDOC_OPTIONS" nil org-pandoc-extra-options 'space)
                   ))

;; Some simple XML escaping code.  I'm surprised this isn't
;; already in emacs somewhere...

(defvar org-pandoc-xml-escapes
  '(("&"  . "&amp;")
    ("'"  . "&apos;")
    ("\"" . "&quot;")
    ("<"  . "&lt;")
    (">"  . "&gt;")))

(defun org-pandoc-escape-xml (string)
  (mapc #'(lambda (item)
            (setq string
                  (replace-regexp-in-string (car item) (cdr item) string)))
        org-pandoc-xml-escapes)
  string)

(defun org-pandoc-make-copyright-string (author email)
  (format "Copyright %s %s%s" (format-time-string "%Y") author
          (if email
              (format " <%s>" email)
            "")))

(defun org-pandoc-template (contents info)
  (let ((title (plist-get info :title))
        (author (plist-get info :author))
        (email (plist-get info :email))
        (description (plist-get info :description))
        (keywords (plist-get info :keywords))
        (date (org-export-get-date info "%Y-%m-%d"))
        (rights (plist-get info :epub-rights)))
    ;; Since the info alist isn't available after the export, build the metadata
    ;; now and put it in a buffer local variable. 
    (if (or (eq org-pandoc-output-format 'epub)
            (eq org-pandoc-output-format 'epub3))
        (let ((xml (concat
                    (when description
                      (format "<dc:description>%s</dc:description>\n" (org-pandoc-escape-xml description)))
                    (format "<dc:rights>%s</dc:rights>\n"
                            (org-pandoc-escape-xml (or rights
                                                       (org-pandoc-make-copyright-string (org-export-data author info) (org-export-data email info)))))
                    (when keywords
                      (format "<dc:subject>%s</dc:subject>\n" (org-pandoc-escape-xml keywords))))))
          (setq org-pandoc---epub-stylesheet-filename (plist-get info :epub-stylesheet))
          (setq org-pandoc---epub-metadata xml)
          (setq org-pandoc---epub-cover-filename (plist-get info :epub-cover)))
      (progn
        (setq org-pandoc---epub-stylesheet-filename nil)
        (setq org-pandoc---epub-metadata nil)
        (setq org-pandoc---epub-cover-filename nil)))
    (setq org-pandoc---command-options (plist-get info :pandoc-options))
    (concat (format "%% %s\n" (org-export-data title info))
            (when (and (plist-get info :with-author)
                       author)
              (format "%% %s\n" (org-export-data author info)))
            (when (plist-get info :with-date)
              (format "%% %s\n" (org-export-data date info)))
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

(defun org-pandoc-run-pandoc (filename outfilename output-format &optional options)
  (let* ((args (list "-t" (symbol-name output-format)
                     "-o" outfilename
                     options
                     filename))
         (command (concat org-pandoc-command " " (mapconcat 'identity args " "))))
    (message "Running pandoc as: %s" command)
    (message "Ran pandoc: %s" (shell-command-to-string command))))

(defun org-pandoc-export-to-file (&optional outfile subtreep visible-only)
  (let ((metadata-file (make-temp-file "org-pandoc" nil ".xml"))
        (pandoc-output (concat (file-name-base outfile) "." (symbol-name org-pandoc-output-format))))
    (org-export-to-file 'pandoc outfile subtreep visible-only)
    ;; I really hate passing info back with global variables, but I don't know how
    ;; else to do it.  Can't use a buffer local variable because the current buffer
    ;; is different in this function than when the export is actually running and
    ;; we have access to the info plist.
    (let ((options (concat (when org-pandoc-output-standalone " -s")
                           (when org-pandoc---epub-cover-filename
                             (format " --epub-cover-image=%s" org-pandoc---epub-cover-filename))
                           (when org-pandoc---epub-stylesheet-filename
                             (format " --epub-stylesheet=%s" org-pandoc---epub-stylesheet-filename))
                           (when org-pandoc---command-options
                             (concat " " org-pandoc---command-options)))))
      (when org-pandoc---epub-metadata
        (with-temp-file metadata-file
          (insert org-pandoc---epub-metadata))
        (setq options (concat options " --epub-metadata=" metadata-file)))
      (org-pandoc-run-pandoc outfile pandoc-output org-pandoc-output-format options))
    (delete-file metadata-file)
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
