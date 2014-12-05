(require 'ac-html)

(defvar fixture-dir (expand-file-name "fixtures" ac-html-test-dir))

(ert-deftest test-ac-html--load-list-from-file ()
  "Test `ac-html--load-list-from-file' loads the list separated by \\n.
If file exist."
  (let* ((file-name (expand-file-name "list.txt" fixture-dir))
         (list-from-file (ac-html--load-list-from-file file-name)))
    (should (equal list-from-file '("foo" "bar" "baz")))))

(ert-deftest test-ac-html--current-html-tag ()
  "Test `ac-html--current-html-tag' correctly scan the tag user is typing on."
  (with-temp-buffer
    (insert "<html><head lang=\"\"\n\n lang=\"\"></head></html>")
    (goto-char 20)
    (should (equal (ac-html--current-html-tag) "head"))))

(ert-deftest test-ac-html--current-html-attribute ()
  "Test `ac-html--current-html-attribute' correctly scan the attribute."
  (with-temp-buffer
    (insert "<input type=\"\"></input>")
    (goto-char 14)
    (should (equal (ac-html--current-html-attribute) "type"))))
