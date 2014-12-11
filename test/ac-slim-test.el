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

(ert-deftest test-ac-html--current-slim-tag ()
  "Test `ac-html--current-slim-tag' correctly scan the tag user is typing on."
  (test-suite-slim-current-thing-scan
   (lambda ()
     (should (equal (ac-html--current-slim-tag) "title")))))

(ert-deftest test-ac-html--current-slim-attribute-candidates ()
  "Test `ac-html--current-slim-attribute-candidates' return correctly."
  (test-suite-slim-current-thing-scan
   (lambda ()
     (should (equal (nth 2 (ac-source-slim-attribute-candidates))
                    "contenteditable")))))
