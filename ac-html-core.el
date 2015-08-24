;;; ac-html-core.el --- auto complete html core      -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>

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

;;

;;; Code:

(require 'auto-complete)
(require 'cl)

;;; Customization

(defgroup auto-complete-html nil
  "HTML Auto Complete."
  :group 'auto-complete
  :prefix "ac-html-")

;;; Unused

(defvar ac-html-user-defined-class-list nil "User defined class list.")

(defvar ac-html-user-defined-id-list nil "User defined id list.")

;;; Variables

(defvar ac-html-data-providers nil "Completion data providers.")

(defvar-local ac-html-enabled-providers nil
  "The enabled data providers of current buffer.")

(defvar-local ac-html-current-tag-function nil
  "The function to find current tag.")

(defvar-local ac-html-current-attr-function nil
  "The function to find current attr.")

;;; Provider

(defmacro ac-html-define-data-provider (name &rest pairs)
  "Define ac-html data provider with this macro."
  (declare (indent 1) (debug t))
  (let (this-provider)
    (setq this-provider (intern name))
    (add-to-list 'ac-html-data-providers this-provider t)
    (cl-loop for (label value) on pairs by #'cddr
             do (put this-provider label value))))

(defmacro ac-html-enable-data-provider (provider)
  "Enable data provider PROVIDER."
  (add-to-list 'ac-html-enabled-providers provider))

(defmacro ac-html-query-data-provider (provider key)
  (get provider key))

;;; Language

;; Macro usage
;; (ac-html-define-ac-source "html"
;;                           :tag-prefix "<\\(.*\\)"
;;                           :attr-prefix "<\\w[^>]*[[:space:]]+\\(.*\\)"
;;                           :attrv-prefix "sdfsdfsdf"
;;                           :current-tag-func
;;                           :current-attr-func)

(defmacro ac-html-define-ac-source (lang &rest pairs)
  "Define ac-html lang with this macro."
  (declare (indent 1) (debug t))
  (let (tag-prefix attr-prefix attrv-prefix current-tag-func current-attr-func)
    (let (label value)
      (while (not (= 0 (length pairs)))
        (setq label (pop pairs))
        (setq value (pop pairs))
        (and (equal :tag-prefix label) (setq tag-prefix value))
        (and (equal :attr-prefix label) (setq attr-prefix value))
        (and (equal :attrv-prefix label) (setq attrv-prefix value))
        (and (equal :current-tag-func label) (setq current-tag-func value))
        (and (equal :current-attr-func label) (setq current-attr-func value))))
    (setq ac-html-current-tag-function current-tag-func)
    (setq ac-html-current-attr-function current-attr-func)
    `(progn
       (ac-define-source ,(format "%s-%s" lang "tag")
         '((candidates . ac-html-all-tag-candidates)
           (prefix . ,tag-prefix)
           (document . ac-html-tag-documentation)
           (symbol . "t")))
       (ac-define-source ,(format "%s-%s" lang "attr")
         '((candidates . ac-html-all-attr-candidates)
           (prefix . ,attr-prefix)
           (document . ac-html-attr-documentation)
           (symbol . "a")))
       (ac-define-source ,(format "%s-%s" lang "attrv")
         '((candidates . ac-html-all-attrv-candidates)
           (prefix . ,attrv-prefix)
           (document . ac-html-attrv-documentation)
           (symbol . "v"))))))

;;; Data

(defun ac-html-all-tag-candidates ()
  "All tag candidates get from data providers."
  (let (list func)
    (dolist (provider ac-html-enabled-providers)
      (setq func (ac-html-query-data-provider provider :tag-func))
      (setq list (-concat list (funcall func))))
    list))

(defun ac-html-all-attr-candidates ()
  "All attr candidates get from data providers."
  (let (list func tag)
    (dolist (provider ac-html-enabled-providers)
      (setq func (ac-html-query-data-provider provider :attr-func))
      (setq tag (funcall ac-html-current-tag-function))
      (setq list (-concat list (funcall func tag))))
    list))

(defun ac-html-all-attrv-candidates ()
  "All attrv candidates get from data providers."
  (let (list func tag attr)
    (dolist (provider ac-html-enabled-providers)
      (setq func (ac-html-query-data-provider provider :attrv-func))
      (setq tag (funcall ac-html-current-tag-function))
      (setq attr (funcall ac-html-current-attr-function))
      (setq list (-concat list (funcall func tag attr))))
    list))

(defun ac-html-tag-documentation (tag)
  "Not documented yet."
  (catch 'return-val
    (let (doc func)
      (dolist (provider ac-html-enabled-providers)
        (setq func (ac-html-query-data-provider provider :tag-doc-func))
        (setq doc (funcall func tag))
        (if doc (throw 'return-val doc))))))

(defun ac-html-attr-documentation (attr)
  "Not documented yet."
  (catch 'return-val
    (let (doc func tag)
      (dolist (provider ac-html-enabled-providers)
        (setq func (ac-html-query-data-provider provider :attr-doc-func))
        (setq tag (funcall ac-html-current-tag-function))
        (setq doc (funcall func tag attr))
        (if doc (throw 'return-val doc))))))

(defun ac-html-attrv-documentation (attrv)
  "Not documented yet."
  (catch 'return-val
    (let (doc func tag attr)
      (dolist (provider ac-html-enabled-providers)
        (setq func (ac-html-query-data-provider provider :attrv-doc-func))
        (setq tag (funcall ac-html-current-tag-function))
        (setq attr (funcall ac-html-current-attr-function))
        (setq doc (funcall func tag attr attrv))
        (if doc (throw 'return-val doc))))))

(provide 'ac-html-core)
;;; ac-html-core.el ends here
