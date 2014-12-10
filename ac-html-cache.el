;;; ac-html-cache.el --- auto complete source for html tags and attributes

;; Copyright (C) 2014 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
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

;; This file provide necessary id and class completion support.

;;; Code:

(defmacro ac-html-ensure-project (&rest body)
  "This macro ensures `projectile' is existing, otherwise just raise error."
  `(if (featurep 'projectile)
       (progn ,@body)
     (error "Projectile is needed to provide id and class auto completion.")))

(defun ac-html-user-cache-dir ()
  "Directory of user cache dir.
If not exist, automatically create."
  (let ((cache-dir (expand-file-name ".ac-html" user-emacs-directory)))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir))
    cache-dir))

(defun ac-html-user-project-cache-dir (project-path)
  "Directory of user project cache dir.
PROJECT-PATH is absolute path of project root."
  (let ((this-cache-dir (expand-file-name
                         (md5 project-path)
                         (ac-html-user-cache-dir))))
    (unless (file-exists-p this-cache-dir)
      (make-directory this-cache-dir))
    this-cache-dir))

(defun ac-html-user-current-project-dir ()
  "Root directory of user's current project.
Currently, use projectile to find project dir."
  (ac-html-ensure-project
   (projectile-project-root)))

(defun ac-html-user-current-project-cache-dir ()
  "User cache dir for current project.
If not exist, automatically create."
  (ac-html-user-project-cache-dir (ac-html-user-current-project-dir)))

(defvar ac-html-html-modes '(web-mode html-mode haml-mode
                                      jade-mode slim-mode)
  "Modes recognized to be html compatible modes. Should provide html
completion to these modes.")

(defvar ac-html-css-modes '(css-mode sass-mode scss-mode
                                     less-mode)
  "Modes recognized to be css compatible modes. Should provide css
completion to these modes.")

(defun ac-html-mode-index-list (mode-list &optional project-dir)
  "Return a list of user project's files that should be opened with
a mode in MODE-LIST."
  (let ((list-to-return ()))
    (ac-html-ensure-project
     (mapc
      (lambda (file)
        (if (member
             (assoc-default file auto-mode-alist 'string-match)
             mode-list)
            (setq list-to-return (cons file list-to-return))))
      (projectile-dir-files
       (or project-dir (ac-html-user-current-project-dir)))))
    list-to-return))

(defun ac-html-user-html-files-index (&optional project-dir)
  "Return a list of user's html compatible files."
  (ac-html-mode-index-list ac-html-html-modes project-dir))

(defun ac-html-user-css-files-index (&optional project-dir)
  "Return a list of user's css compatible files."
  (ac-html-mode-index-list ac-html-css-modes project-dir))

(defun ac-html-cache-bundle-location (source project-cache-dir)
  "Take SOURCE and PROJECT-CACHE-DIR, then return a directory contain all
cache files for the SOURCE file.
For example:
SOURCE = \"app/index.html\", PROJECT-CACHE-DIR = \"~/.emacs.d/.ac-html/md5/\"
returns a directory named: \"~/.emacs.d/.ac-html/md5/app/index.html/."
  (ac-html-ensure-project
   (let ((file-name (expand-file-name source project-cache-dir)))
     (unless (file-exists-p file-name)
       (make-directory file-name t))
     file-name)))

(defun ac-html-class-file-location (source project-cache-dir)
  "Take SOURCE and PROJECT-CACHE-DIR, then return class-file.
PROJECT-CACHE-DIR should be absolute directory.
SOURCE should be file name relative to project dir."
  (ac-html-ensure-project
   (expand-file-name "class.list"
                     (ac-html-cache-bundle-location
                      source project-cache-dir))))

(defun ac-html-id-file-location (source project-cache-dir)
  "Same as `ac-html-class-file-location', except this function returns
id file location."
  (ac-html-ensure-project
   (expand-file-name "id.list"
                     (ac-html-cache-bundle-location
                      source project-cache-dir))))

(defun ac-html-md5-file-location (source project-cache-dir)
  "Same as `ac-html-class-file-location' and `ac-html-id-file-location',
this function returns md5 hash file location."
  (ac-html-ensure-project
   (expand-file-name "md5.hash"
                     (ac-html-cache-bundle-location
                      source project-cache-dir))))

(defun ac-html-file-full-name (source project-dir)
  "Return full path of SOURCE."
  (expand-file-name source project-dir))

(defun ac-html-md5-of-file (file-full-name)
  "Return md5 cauculation of file at source"
  (with-current-buffer (find-file-noselect file-full-name)
    (let (hash)
      (setq hash (md5 (current-buffer)))
      (kill-buffer (current-buffer))
      hash)))

(defun ac-html-cached-md5-of-file
    (source &optional project-dir project-cache-dir)
  "Must specify whether PROJECT-DIR or PROJECT-CACHE-DIR."
  (setq project-cache-dir
        (or project-cache-dir (ac-html-user-project-cache-dir project-dir)))
  (or project-cache-dir (error "ac-html-cached-md5-of-file: argument error."))
  (with-current-buffer
      (find-file-noselect (ac-html-md5-file-location source project-cache-dir))
    (let (hash)
      (setq hash
            (save-restriction
              (widen)
              (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer (current-buffer))
      hash)))

(defun ac-html-hash-and-save-md5-file (source project-dir &optional location)
  "MD5 hash SOURCE file in PROJECT-DIR, and place its md5 into corresponding
cache file.
LOCATION argument is in discuss and not implemented."
  (let ((hash-string
         (ac-html-md5-of-file (ac-html-file-full-name source project-dir)))
        (file-location
         (ac-html-md5-file-location
          source (ac-html-user-project-cache-dir project-dir))))
    (with-current-buffer
        (find-file-noselect file-location)
      (erase-buffer)
      (insert hash-string)
      (save-buffer)
      (kill-buffer))))

(defun ac-html-file-need-index (source project-dir)
  "Compare SOURCE file content md5 with cached md5. If same, return nil.
Otherwise, return t."
  (not (string=
        (ac-html-md5-of-file (ac-html-file-full-name source project-dir))
        (ac-html-cached-md5-of-file source project-dir))))

(defun ac-html-hash-and-save-md5-file-if-needed (source project-dir)
  "MD5 hash SOURCE file if needed. This function is just combination of
`ac-html-hash-and-save-md5-file' and `ac-html-file-need-index'."
  (if (ac-html-file-need-index source project-dir)
      (ac-html-hash-and-save-md5-file source project-dir)))

(defun ac-html-generate-corresponding-class-file
    (source project-dir &optional location)
  "Generate class list file from SOURCE file. SOURCE file can be
html compatible file or css compatible file.
Return location of class file."
  )
(defun ac-html-generate-corresponding-id-file
    (source project-dir &optional location)
  "Generate id list file from SOURCE file. SOURCE file can be
html compatible file or css compatible file.
Return location of id file.")

(defvar ac-html-html-class-re-list nil
  "")

(defvar ac-html-html-id-re-list nil
  "")

(defvar ac-html-css-class-re-list nil
  "")

(provide 'ac-html-cache)
;; ac-html-cache.el ends here
