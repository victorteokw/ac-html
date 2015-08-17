;;; ac-html.el --- auto complete source for html tags and attributes

;; Copyright (C) 2014 - 2015 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Version: 0.31
;; Keywords: html, auto-complete, slim, haml, jade
;; Package-Requires: ((auto-complete "1.4") (web-completion-data "0.1"))
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
(require 'cl)

(require 'ac-html-core)

;;; Customization

(defgroup auto-complete-html nil
  "HTML Auto Complete."
  :group 'auto-complete
  :prefix "ac-html-")

(defcustom ac-html-complete-css t
  "Enable style attribute CSS autocomplete."
  :group 'auto-complete-html
  :type 'boolean)

(defcustom ac-html-summary-truncate-length 10
  "Truncation length for type summary."
  :type 'integer
  :group 'auto-complete-html)






;;; auto complete HTML for html-mode and web-mode

;; ac-source functions

(defun ac-html-current-tag ()
  "Return current html tag user is typing on."
  (save-excursion
    (re-search-backward "<\\(\\w+\\)[[:space:]]+" nil t)
    (match-string 1)))

(defun ac-html-current-attribute ()
  "Return current html tag's attribute user is typing on."
  (save-excursion
    (re-search-backward "[^a-z-]\\([a-z-]+\\)=" nil t)
    (match-string 1)))

(defun ac-html-value-prefix ()
  (if (re-search-backward "\\w=[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

;;;###autoload
(ac-define-source ac-source-html-tag
  '((candidates . ac-html-html-tag-candidates)
    (prefix . "<\\(.*\\)")
    (symbol . "t")))

;;;###autoload
(ac-define-source ac-source-html-attribute
  '((candidates . ac-source-html-attribute-candidates)
    (prefix . "<\\w[^>]*[[:space:]]+\\(.*\\)")
    (symbol . "a")))

;;;###autoload
(ac-define-source ac-source-html-attribute-value
  '((candidates . ac-source-html-attribute-value-candidates)
    (prefix . ac-html-value-prefix)
    (document . ac-source-html-attribute-value-document)
    (symbol . "v")))

;;;###autoload
(defun ac-html-enable ()
  "Add ac-html sources into ac-sources and enable auto-comple-mode."
  (interactive)
  (mapc (lambda (source)
          (if (not (memq source ac-sources))
              (add-to-list 'ac-sources source)))
        '(ac-source-html-attribute-value ac-source-html-attribute
                                         ac-source-html-tag))

  ;; ac-source-jade-attribute-value complete in font-lock-string-face,
  ;; must not be disabled
  (make-local-variable 'ac-disable-faces)
  (setq ac-disable-faces (remove 'font-lock-string-face ac-disable-faces))
  (auto-complete-mode t))

(provide 'ac-html)
;;; ac-html.el ends here
