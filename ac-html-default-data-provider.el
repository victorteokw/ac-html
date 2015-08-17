(require 'web-completion-data)

(defvar ac-html-tags-list nil "The list of tags.")
(defvar ac-html-global-attributes nil "The list of global attrs.")

(defun ac-html--load-list-from-file (filepath)
  "Return a list separated by \\n from FILEPATH."
  (with-current-buffer (find-file-noselect filepath)
    (unwind-protect
        (split-string
         (save-restriction
           (widen)
           (buffer-substring-no-properties (point-min) (point-max)))
         "\n" t)
      (kill-buffer))))

(defun ac-html--read-file (file-in-source-dir)
  "Return string content of FILE-IN-SOURCE-DIR from `web-completion-data-sources'."
  (let ((file (cdr (nth 0 (ac-html--all-files-named file-in-source-dir)))))
    ;; Just read from the first file.
    (when file
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun ac-html--tags ()
  (if ac-html-tags-list
      ac-html-tags-list
    (setq ac-html-tags-list
          (ac-html--flatten
           (mapcar (lambda (source-name-and-file-path)
                     (ac-html--make-popup-items
                      (car source-name-and-file-path)
                      (ac-html--load-list-from-file (cdr source-name-and-file-path))
                      (lambda (tag-name)
                        (let ((doc (ac-html--read-file
                                    (concat "html-tag-short-docs/" tag-name))))
                          (if doc
                              doc
                            "Currently not documented.")))))
                   (ac-html--all-files-named "html-tag-list"))))
    ac-html-tags-list))

(defun ac-html--make-popup-items (summary items documentation)
  "Make popup-item for each item with SUMMARY.

SUMMARY will be truncated to `ac-html-summary-truncate-length'.

ITEMS is a list of string where name and documentation are
separated by one space.
Documentation newlines are escaped by \"\\n\".

If item have no inline documentation, DOCUMENTATION will be used.
DOCUMENTATION is string or function."
  (let ((truncated-summary
         (truncate-string-to-width
          summary ac-html-summary-truncate-length 0 nil nil)))
    (mapcar (lambda (item)
              (if (string-match "\\(.*?\\) \\(.*\\)" item)
                  (popup-make-item (match-string 1 item)
                                   :summary truncated-summary
                                   :document (replace-regexp-in-string
                                              "\\\\n" "\n"
                                              (match-string 2 item)))
                (popup-make-item item
                                 :summary truncated-summary
                                 :document documentation)))
            items)))



(defun ac-html--all-files-named (file-name)
  "Get a list of file named FILE-NAME in all directory specified by
 `web-completion-data-sources'.

Returns an alist. car is source name, cdr is the file path."
  (let (return-files source-dir-path)
    (mapc (lambda (name-dir-cons-cell)
            (setq source-dir-path (cdr name-dir-cons-cell))
            (setq source-dir-path
                  (cond ((stringp source-dir-path) source-dir-path)
                        ((and (symbolp source-dir-path)
                              (boundp source-dir-path))
                         (symbol-value source-dir-path))
                        (t
                         (error "[ac-html] invalid element %s in\
 `web-completion-data-sources'" source-dir-path))))
            (when source-dir-path
              (setq source-dir-path (expand-file-name file-name source-dir-path))
              (when (file-exists-p source-dir-path)
                (add-to-list 'return-files (cons (car name-dir-cons-cell) source-dir-path))
                )))
          web-completion-data-sources)
    return-files))


(defun ac-html--attribute-candidates (tag-string document)
  "Attribute candidates for auto complete."
  (unless (ac-html--check-string-face)
    (let* ((items
            (mapcar (lambda (source-name-and-file-path)
                      (ac-html--make-popup-items
                       (concat (car source-name-and-file-path) ", G")
                       (ac-html--load-list-from-file
                        (cdr source-name-and-file-path))
                       document
                       ))
                    (ac-html--all-files-named "html-attributes-list/global"))))
      (add-to-list 'items
                   (mapcar (lambda (source-name-and-file-path)
                             (ac-html--make-popup-items
                              (car source-name-and-file-path)
                              (ac-html--load-list-from-file
                               (cdr source-name-and-file-path))
                              document
                              ))
                           (ac-html--all-files-named
                            (concat "html-attributes-list/" tag-string))))
      (ac-html--flatten items))))

(defun ac-html--attribute-documentation (attribute tag)
  (let* ((doc-file (format "html-attributes-short-docs/%s-%s" tag attribute))
         (doc (ac-html--read-file doc-file)))
    (if doc
        doc
      (progn
        (setq doc-file (format "html-attributes-short-docs/global-%s" attribute))
        (setq doc (ac-html--read-file doc-file))
        (if doc
            doc
          "Currently not documented.")))))

(defun ac-source--html-values-internal (tag-string attribute-string)
  "Read html-stuff/html-attributes-complete/global-<ATTRIBUTE>
and html-stuff/html-attributes-complete/<TAG>-<ATTRIBUTE> files

Those files may have documantation delimited by \" \" symbol."

  (let* ((items (mapcar
                 (lambda (alist)
                   (ac-html--make-popup-items
                    (concat (car alist) ", G")
                    (ac-html--load-list-from-file (cdr alist))
                    nil))
                 (ac-html--all-files-named
                  (concat "html-attributes-complete/global-"
                          attribute-string)))))
    (add-to-list 'items
                 (mapcar (lambda (alist)
                           (ac-html--make-popup-items
                            (car alist)
                            (ac-html--load-list-from-file (cdr alist))
                            nil))
                         (ac-html--all-files-named
                          (format "html-attributes-complete/%s-%s" tag-string
                                  attribute-string))))
    (ac-html--flatten items)))

(defun ac-source--html-attribute-values (tag-string attribute-string)
  (if (and ac-html-complete-css
           (string= attribute-string "style")
           (< ;; make sure that quote openned before ac-css-prefix
            (1+ (save-excursion (re-search-backward "\"" nil t)))
            (or (ac-css-prefix) 0)))
      ;; TODO: how to compare numbers with possible nil?
      (ac-html--make-popup-items "CSS" (ac-css-property-candidates) nil)
    (ac-source--html-values-internal tag-string attribute-string)))

(ac-html-define-data-provider "ac-html-default-data-provider"
  :tag-func 'ac-html-default-tags
  :attr-func 'ac-html-default-attrs
  :attrv-func 'ac-html-default-attrvs
  :id-func 'ac-html-default-ids
  :class-func 'ac-html-default-classes
  :tag-doc-func 'ac-html-default-tag-doc
  :attr-doc-func 'ac-html-default-attr-doc
  :attrv-doc-func 'ac-html-default-attrv-doc
  :id-doc-func 'ac-html-default-id-doc
  :class-doc-func 'ac-html-default-class-doc)

(provide 'ac-html-default-data-provider)
;;; ac-html-default-data-provider.el ends here
