;;; SLIM test

(require 'ac-slim)

(defun test-suite-slim-current-thing-scan (body)
  "Test suite is for ac-slim."
  (unwind-protect
      (progn
        (with-temp-buffer
          (insert "html\n  head\n    title This Title\n  body\n    div")
          (goto-char 30)
          (funcall body)))))

(ert-deftest test-ac-slim-current-tag ()
  "Test `ac-slim-current-tag' correctly scan the tag user is typing on."
  (test-suite-slim-current-thing-scan
   (lambda ()
     (should (equal (ac-slim-current-tag) "title")))))
