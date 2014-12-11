(defvar ac-html-test-dir (file-name-directory load-file-name))
(defvar ac-html-root-dir (append ac-html-test-dir ".."))

(mapc (lambda (p)
        (add-to-list 'load-path p))
      (list ac-html-test-dir ac-html-root-dir))

(load "ac-html-test")
(load "ac-haml-test")
(load "ac-slim-test")
(load "ac-jade-test")
