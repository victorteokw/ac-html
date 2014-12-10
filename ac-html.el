;;; ac-html.el --- auto complete source for html tags and attributes

;; Copyright (C) 2014 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Version: 0.2.0
;; Keywords: html, auto-complete, rails, ruby
;; Package-Requires: ((auto-complete "1.4"))
;; URL: https://github.com/cheunghy/ac-html

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration:
;;
;; Add to hook `ac-html-enable'
;; 
;; (add-hook 'html-mode-hook 'ac-html-enable)
;;
;; If you are using web-mode:
;;
;; (add-to-list 'web-mode-ac-sources-alist
;;              '("html" . (
;;                          ;; attribute-value better to be first
;;                          ac-source-html-attribute-value
;;                          ac-source-html-tag
;;                          ac-source-html-attribute)))
;;
;; `ac-html-enable' remove from list ac-disable-faces 'font-lock-string-face,
;; so if you wish manually add ac-source-html-attribute-value, etc, you may need
;; customize ac-disable-faces too.
;;; Code:

(require 'auto-complete)
(require 'auto-complete-config)
(require 'cl)

(defconst ac-html-package-dir (file-name-directory load-file-name)
  "The directory where `ac-html' package exists.")

(defconst ac-html-basic-source-dir
  (expand-file-name "html-stuff" ac-html-package-dir)
  "The directory where basic source of `ac-html' exists.")

;;; Customization

(defgroup auto-complete-html nil
  "HTML Auto Complete."
  :group 'auto-complete
  :prefix "ac-html-")


(defvar ac-html-installed-html-stuff nil)
(setq ac-html-installed-html-stuff
      (when load-file-name
        (concat (file-name-directory load-file-name) "html-stuff")))

(defcustom ac-html-source-dirs
  '(
    ("html" . ac-html-installed-html-stuff)
    ("test" . "~/work/tests/my-html-stuff")
    )
  "Alist specifying html stuff name, and directory."
  :type 'alist
  :group 'auto-complete-html)

(defcustom ac-html-style-css t
  "Enable style attribute CSS autocomplete.
If not nil no need 'ac-source-css-property in web-mode-ac-sources-alist for web-mode (\"html\" list dot pair)"
  :group 'auto-complete-html
  :type 'boolean)

(defcustom ac-html-summary-truncate-length 10
  "Truncation length for type summary."
  :type 'integer
  :group 'auto-complete-html)

;;; Variables

(defvar ac-html-root-element-list
  (list
   "html" "!DOCTYPE html"))

(defvar ac-html-first-child-element-list
  (list
   "head" "body"))

(defvar ac-html-unique-element-list
  (list
   "html" "head" "body" "title"))

(defvar ac-html-block-level-element-list
  (list
   "address" "article" "aside" "audio" "blockquote" "canvas" "dd" "div" "dl"
   "fieldset" "figcaption" "figure" "footer" "form" "h1" "h2" "h3" "h4" "h5"
   "h6" "header" "hgroup" "hr" "noscript" "ol" "output" "p" "pre" "section"
   "table" "tfoot" "ul" "video"))

(defvar ac-html-inline-element-list
  (list
   "b" "big" "i" "small" "tt"
   "abbr" "acronym" "cite" "code" "dfn" "em" "kbd" "strong" "samp" "var"
   "a" "bdo" "br" "img" "map" "object" "q" "script" "span" "sub" "sup"
   "button" "input" "label" "select" "textarea"))



(defvar ac-html-user-defined-class-list
  '())

(defvar ac-html-user-defined-id-list
  '())

(defvar ac-html-string-check-faces '(font-lock-string-face web-mode-html-attr-value-face)
  "List of string faces")

;;; Functions

(defun ac-html--load-list-from-file (filepath)
  "Return a list separated by \\n from FILEPATH."
  (with-current-buffer (find-file-noselect filepath)
    (split-string (save-restriction
		    (widen)
		    (buffer-substring-no-properties
		     (point-min) (point-max)))
		  "\n" t)))

(defun ac-html--get-files (file-name)
  "Alist of all expanded filename that exists in `ac-html-source-dirs' dirs.

Return alist:
head - is car of `ac-html-source-dirs', a name of html-stuff
tail - filename"
  (let (return-files)			; acumulate no nil here
    (mapc  #'(lambda (alist)
		 (let* ((path (cdr alist)))
		   (setq path 
			 (cond ((stringp path) path)
			       ((and (symbolp path)
				     (boundp path)
				     (stringp (symbol-value path)))
				(symbol-value path))
			       (t
				(error "[ac-html] invalid element %s in `ac-html-source-dirs'" path))))
		   (setq path (expand-file-name file-name path))
		   (when (file-exists-p path)
		     (add-to-list 'return-files (cons (car alist) path))
		     )))
	   ac-html-source-dirs)
    return-files))

;; http://rosettacode.org/wiki/Flatten_a_list can't find elisp ready same func
(defun ac-html--flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'ac-html--flatten structure))))

(defun ac-html--make-popup-items (summary items document)
  "Make popup-item for each item with summary SUMMARY

ITEMS are string where item and it document are separated by one space.
Document part newlines escaped by \"\\n\".

If item have no inline document, DOCUMENT will beused. DOCUMENT must be string or function.

Truncate SUMMARY to `ac-html-summary-truncate-length'."
  (let ((truncated-summary (truncate-string-to-width
			    summary ac-html-summary-truncate-length 0 nil nil)))
    (mapcar #'(lambda (item)
		(if (string-match "\\(.*?\\) \\(.*\\)" item)
		    (popup-make-item (match-string 1 item)
				     :summary truncated-summary
				     :document (replace-regexp-in-string "\\\\n" "\n"
									 (match-string 2 item)))
		  (popup-make-item item
				   :summary truncated-summary
				   :document document)))
	    items)))

(defun ac-html--find-file (filename)
  "Find and read FILENAME from `ac-html-source-dirs'"
  (let ((file (cdr (car (ac-html--get-files filename))))) ; first file that exists
    (when file
      (with-temp-buffer
	(insert-file-contents file)
	(buffer-string)))))

(defun ac-html--tags ()
  (ac-html--flatten
   (mapcar #'(lambda (alist)
	       (ac-html--make-popup-items (car alist)
					  (ac-html--load-list-from-file (cdr alist))
					  #'(lambda (symbol) 
					      (let ((doc (ac-html--find-file (concat "html-tag-short-docs/" symbol))))
						(if doc
						    doc
						  "Currently not documented."))
					      )
					  ))
	   (ac-html--get-files "html-tag-list"))))

(defun ac-html--attribute-documentation (attribute tag)
  (let* ((doc-file (format "html-attributes-short-docs/%s-%s" tag attribute))
	 (doc (ac-html--find-file doc-file)))
    (if doc
	doc
      (progn
	(setq doc-file (format "html-attributes-short-docs/global-%s" attribute))
	(setq doc (ac-html--find-file doc-file))
	(if doc
	    doc
	  "Currently not documented.")
	))))

(defun ac-html--check-string-face ()
  "t if text's face(s) at point is in `ac-html-string-check-faces'."
  (let ((faces (get-text-property (point) 'face)))
    (if (listp faces) 			; slim-mode define list of string-face (bug), so intersect
	(intersection faces ac-html-string-check-faces)
      (memq faces ac-html-string-check-faces) ;faces is atom
      )))

(defun ac-html--attribute-candidates (tag-string document)
  "Attribute candidates. Summary \"G\" if global."
  (unless (ac-html--check-string-face)
    (let* ((items
	    (mapcar #'(lambda (alist)
			(ac-html--make-popup-items (concat (car alist) ", G")
						   (ac-html--load-list-from-file (cdr alist))
						   document
						   ))
		    (ac-html--get-files "html-attributes-list/global"))))
      (add-to-list 'items
		   (mapcar #'(lambda (alist)
			       (ac-html--make-popup-items (car alist)
							  (ac-html--load-list-from-file (cdr alist))
							  document
							  ))
			   (ac-html--get-files (concat "html-attributes-list/" tag-string))))     
      (ac-html--flatten items))))

(defun ac-source--html-values-internal (tag-string attribute-string)
  "Read html-stuff/html-attributes-complete/global-<ATTRIBUTE>
and html-stuff/html-attributes-complete/<TAG>-<ATTRIBUTE> files

Those files may have documantation delimited by \" \" symbol."
  (let* ((items (mapcar #'(lambda (alist)
			    (ac-html--make-popup-items (concat (car alist) ", G")
						       (ac-html--load-list-from-file (cdr alist))
						       nil))
			(ac-html--get-files (concat "html-attributes-complete/global-" attribute-string)))))
    (add-to-list 'items
		 (mapcar #'(lambda (alist)
			     (ac-html--make-popup-items (car alist)
							(ac-html--load-list-from-file (cdr alist))
							nil))
			 (ac-html--get-files (format "html-attributes-complete/%s-%s" tag-string attribute-string))))
    (ac-html--flatten items)))

(defun ac-source--html-attribute-values (tag-string attribute-string)
  (if (and ac-html-style-css
	   (string= attribute-string "style")
	   (<				; make sure that quote openned before ac-css-prefix
	    (1+ (save-excursion (re-search-backward "\"" nil t)))
	    (or (ac-css-prefix) 0)))	; TODO: how to compare numbers with possible nil?
      (ac-html--make-popup-items "CSS" (ac-css-property-candidates) nil)
    (ac-source--html-values-internal tag-string attribute-string)))

;; ac-source functions

(defun ac-html-current-tag ()
  "Return current html tag user is typing on."
  (save-excursion (re-search-backward "<\\(\\w+\\)[[:space:]]+" nil t))
  (match-string 1))

(defun ac-html-current-attribute ()
  "Return current html tag's attribute user is typing on."
  (save-excursion (re-search-backward "[^a-z-]\\([a-z-]+\\)=" nil t))
  (match-string 1))

(defun ac-source-html-tag-candidates ()
  (ac-html--tags))

(defun ac-source-html-attribute-candidates ()
  (ac-html--attribute-candidates (ac-html-current-tag)
				 #'(lambda (symbol)
				     (ac-html--attribute-documentation symbol (ac-html-current-tag)))))

(defun ac-source-html-attribute-value-candidates ()
  (ac-source--html-attribute-values
   (ac-html-current-tag) (ac-html-current-attribute)))

(defun ac-html-value-prefix ()
  (if (re-search-backward "\\w=[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

(defvar ac-source-html-tag
  '((candidates . ac-source-html-tag-candidates)
    (prefix . "<\\(.*\\)")
    (symbol . "t")))

(defvar ac-source-html-attribute
  '((candidates . ac-source-html-attribute-candidates)
    (prefix . "<\\w[^>]*[[:space:]]+\\(.*\\)")
    (symbol . "a")))

(defvar ac-source-html-attribute-value
  '((candidates . ac-source-html-attribute-value-candidates)
    (prefix . ac-html-value-prefix)
    (symbol . "v")
))

(defun ac-html-enable ()
  "Add ac-html sources into ac-sources and enable auto-comple-mode"
  (interactive)
  (mapc (lambda (source)
	  (if (not (memq source ac-sources))
	      (add-to-list 'ac-sources source)))
	'(ac-source-html-attribute-value ac-source-html-attribute ac-source-html-tag))

  ;; ac-source-jade-attribute-value complete in font-lock-string-face, must not be disabled
  (make-local-variable 'ac-disable-faces)
  (setq ac-disable-faces (remove 'font-lock-string-face ac-disable-faces))
  (auto-complete-mode t))

(provide 'ac-html)
;;; ac-html.el ends here
