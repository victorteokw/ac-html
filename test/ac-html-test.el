(require 'ac-html)

;;; Core test

(ert-deftest test-ac-html--load-list-from-file ()
  "Test `ac-html--load-list-from-file'.
`ac-html--load-list-from-file' should load list separated by \\n from file."
  (let* ((file-name (expand-file-name "html-tag-list"
                                      ac-html-basic-source-dir))
         (list-from-file (ac-html--load-list-from-file file-name)))
    (should (equal (nth 0 list-from-file) "a"))
    (should (equal (nth 1 list-from-file) "abbr"))
    (should (equal (last list-from-file 2) '("wbr" "xmp")))))

(ert-deftest test-ac-html-all-element-list ()
  "Test `ac-html-all-element-list'.
It should return correctly."
  (should-not (equal ac-html-all-element-list '()))
  (should (equal (car ac-html-all-element-list) "a"))
  (should (equal (last ac-html-all-element-list 2) '("wbr" "xmp")))
  )

(ert-deftest test-ac-source--html-tag-documentation ()
  "Test `ac-source--html-tag-documentation'.
It should return correctly."
  (should (string-prefix-p "The HTML <a> Element (or t"
                           (ac-source--html-tag-documentation "a"))))

(ert-deftest test-ac-html--attribute-documentation ()
  "Test `ac-html--attribute-documentation'.
It should return correctly."
  ;; Local attribute
  (should (string= "title\n\nSpecifies alternative style sheet sets."
                   (ac-html--attribute-documentation "title" "style")))
  ;; Global attribute
  (should (string-prefix-p "Text to be displayed in a tooltip"
                           (ac-html--attribute-documentation "title" "div"))))

(ert-deftest test-ac-html--attribute-candidates ()
  "Test `ac-html--attribute-candidates'.
It should return correctly."
  (should (member "loop" (ac-html--attribute-candidates "audio")))
  (should (member "class" (ac-html--attribute-candidates "audio")))
  (should-not (member "autoplay" (ac-html--attribute-candidates "div")))
  (should (member "class" (ac-html--attribute-candidates "div"))))

(ert-deftest test-ac-source--html-values-internal ()
  "Test `ac-source--html-values-internal'.
It should return correctly."
  (should (member "_blank Load in a new window"
                  (ac-source--html-values-internal "a" "target"))))

(ert-deftest test-ac-source--html-attribute-values ()
  "Test `ac-source--html-attribute-values.'
It should return correctly."
  (should (member "_blank"
                  (ac-source--html-attribute-values "a" "target"))))

(ert-deftest test-ac-source--html-attribute-value-document ()
  "Test `ac-source--html-attribute-value-document.'
It should return correctly."
  (should (string= "Load in a new window"
             (ac-source--html-attribute-value-document
              "_blank" "a" "target"))))



;;; HTML test

(defun test-suite-html-current-thing-scan (body)
  "This test suite is for
`ac-html--current-html-tag'
`ac-html--current-html-attribute'."
  (unwind-protect
      (progn
        (with-temp-buffer
          (insert "<html><head lang=\"\"\n\n lang=\"\"></head></html>")
          (funcall body)))))

(ert-deftest test-ac-html--current-html-tag ()
  "Test `ac-html--current-html-tag' correctly scan the tag user is typing on."
  (test-suite-html-current-thing-scan
   (lambda ()
     (goto-char 20)
     (should (equal (ac-html--current-html-tag) "head")))))

(ert-deftest test-ac-html--current-html-attribute ()
  "Test `ac-html--current-html-attribute' correctly scan the attribute."
  (test-suite-html-current-thing-scan
   (lambda ()
     (goto-char 29)
     (should (equal (ac-html--current-html-attribute) "lang")))))
