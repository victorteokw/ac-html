;;; HAML test

(require 'ac-haml)

(defun test-suite-haml-current-thing-scan (body)
  "This test suite is for 
`ac-html--current-haml-tag'."
  (unwind-protect
      (progn
        (with-temp-buffer
          (insert "%html\n  %head\n    %title This Title\n  %body\n    %div")
          (goto-char 28)
          (funcall body)))))

(ert-deftest test-ac-html--current-haml-tag ()
  "Test `ac-html--current-haml-tag' correctly scan the tag user is typing on."
  (test-suite-haml-current-thing-scan
   (lambda ()
     (should (equal (ac-html--current-haml-tag) "title")))))

(ert-deftest test-ac-source-haml-attribute-candidates ()
  "Test `ac-source-haml-attribute-candidates' return correctly."
  (test-suite-haml-current-thing-scan
   (lambda ()
     (should (equal (nth 2 (ac-source-haml-attribute-candidates))
                    "contenteditable")))))
