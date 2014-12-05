(require 'ac-html)

(setq ac-html-basic-source-dir (expand-file-name "sources" fixture-dir))

;;; Core test

(ert-deftest test-ac-html--load-list-from-file ()
  "Test `ac-html--load-list-from-file' loads the list separated by \\n.
If file exist."
  (let* ((file-name (expand-file-name "list.txt" fixture-dir))
         (list-from-file (ac-html--load-list-from-file file-name)))
    (should (equal list-from-file '("foo" "bar" "baz")))))

(ert-deftest test-ac-html-all-element-list ()
  "Test the array correct or not."
  (should-not (equal ac-html-all-element-list '()))
  (should (equal (car ac-html-all-element-list) "a"))
  (should (equal (last ac-html-all-element-list 2) '("wbr" "xmp")))
  )

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

