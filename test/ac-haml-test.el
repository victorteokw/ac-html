;;; HAML test

(require 'ac-haml)

(defun test-suite-haml-current-thing-scan (body)
  "This test suite is for ac-haml."
  (unwind-protect
      (progn
        (with-temp-buffer
          (insert "%html\n  %head\n    %title This Title\n  %body\n    %div")
          (goto-char 28)
          (funcall body)))))

(ert-deftest test-ac-html--current-haml-tag ()
  "Test `ac-haml-current-tag' correctly scan the tag user is typing on."
  (test-suite-haml-current-thing-scan
   (lambda ()
     (should (equal (ac-haml-current-tag) "title")))))
