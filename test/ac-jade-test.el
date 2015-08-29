;;; JADE test

(require 'ac-jade)

(defun test-suite-jade-current-thing-scan (body)
  "This test suite is for ac-jade."
  (unwind-protect
      (progn
        (with-temp-buffer
          (insert "html\n  head\n    title= This Title\n  body\n    div")
          (goto-char 30)
          (funcall body)))))

(ert-deftest test-ac-html--current-jade-tag ()
  "Test `ac-jade-current-tag' correctly scan the tag user is typing on."
  (test-suite-jade-current-thing-scan
   (lambda ()
     (should (equal (ac-jade-current-tag) "title")))))
