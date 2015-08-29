(defvar ac-html-test-path (file-name-directory load-file-name))
(defvar ac-html-root-path (append ac-html-test-path ".."))

(mapc (lambda (p) (add-to-list 'load-path p))
      (list ac-html-test-path ac-html-root-path))

(require 'ac-html)
(require 'ert-expectations)

(dolist (test-file (or argv (directory-files ac-html-test-path t "-test.el$")))
  (load test-file nil t))

(ert-run-tests-batch-and-exit)
